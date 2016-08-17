package iliad

import iliad.implicits._

import fs2._
import fs2.util._

import cats._
import cats.data._
import cats.implicits._

import spire.implicits._

import com.typesafe.scalalogging._

import scala.concurrent.duration._

trait EventStream extends LazyLogging {

  private implicit val S: Strategy = Strategy.fromFixedDaemonPool(1, "worker")

  private def baseStream[A](register: (A => Unit) => Unit): Stream[Task, A] =
    Stream.eval(async.boundedQueue[Task, A](10)).flatMap { q =>
      register { (a: A) =>
        q.enqueue1(a)
          .unsafeRunAsync(_.toXor match {
            case Xor.Left(err) => logger.error(s"Error registering event $err")
            case _ =>
          })
      }
      q.dequeue
    }

  def eventStream: Stream[Task, InputEvent] = baseStream(EventHandler.onEvent)
}

sealed trait InputEvent

object InputEvent {
  case class Point(at: Long, x: Float, y: Float) {
    def position: Vec2f = v"$x $y"

    /** window coordinates range from -1f to 1f */
    def windowCoord: Vec2f = (position :* 2f) - 1f
  }

  case class Tap(point: Point) extends InputEvent {
    lazy val at: Long = point.at
    
    def position: Vec2f = point.position

    /** window coordinates range from -1f to 1f */
    def windowCoord: Vec2f = point.windowCoord
  }

  case class DragStarted(start: Point, current: Point) extends InputEvent
  case class DragContinued(points: List[InputEvent.Point])
      extends InputEvent {
    def start: Point = points.toList.last
    def end: Point = points.head
    def distance: Float = (end.position - start.position).norm
  }
  case class DragBecameSwipe(points: List[InputEvent.Point])
      extends InputEvent {
    lazy val start: Point = points.toList.last
    lazy val end: Point = points.head
    lazy val distance: Float = {
      val dx = (end.x - start.x).toDouble
      val dy = (end.y - start.y).toDouble
      Math.sqrt(dx * dx + dy * dy).toFloat
    }
    def duration: Long = end.at - start.at
    def direction: Vec2f = (end.position - start.position).normalize
    def isDown(acceptance: Float): Boolean = direction ⋅ v"0f -1f" > acceptance
    def isLeft(acceptance: Float): Boolean = direction ⋅ v"-1f 0f" > acceptance
    def isRight(acceptance: Float): Boolean = direction ⋅ v"1f 0f" > acceptance
    def isUp(acceptance: Float): Boolean = direction ⋅ v"0f 1f" > acceptance
  }

  case class DragFinished(points: List[InputEvent.Point]) extends InputEvent

  def distance(s: Point, e: Point): Float = {
    val dx = (e.x - s.x).toDouble
    val dy = (e.y - s.y).toDouble
    Math.sqrt(dx * dx + dy * dy).toFloat
  }
}

import InputEvent._

#+x11
import com.sun.jna.platform.unix.X11._
import iliad.platform.unix.X11

object EventHandler {

  type Callback[A] = A => Unit
  def zero[A](a: A): Unit = {}

  private var eventCallback: Callback[InputEvent] = EventHandler.zero

  def onEvent(cb: Callback[InputEvent]): Unit = eventCallback = cb

  private var recogniser: EventRecogniser = EventRecogniser.Blank

  def handleEvent(e: XEvent, width: Int, height: Int): Unit = {
    val (next, eventOpt) = recogniser.handle(e)(width, height)
    eventOpt.foreach(eventCallback)
    recogniser = next
  }
}

sealed trait EventRecogniser {
  def handle(e: XEvent)(width: Int,
                        height: Int): (EventRecogniser, Option[InputEvent])
}

object EventRecogniser {

  val minDt = ((1 / 30) seconds).toMillis

  private def buttonEvent(e: XEvent,
                          width: Int,
                          height: Int): InputEvent.Point = {
    e.readField("xbutton")
    e.xbutton.readField("x")
    e.xbutton.readField("y")
    val xFraction = e.xbutton.x.toFloat / width.toFloat
    val yFraction = 1f - e.xbutton.y.toFloat / height.toFloat
    InputEvent.Point(System.currentTimeMillis, xFraction, yFraction)
  }

  private def motionEvent(e: XEvent,
                          width: Int,
                          height: Int): InputEvent.Point = {
    e.readField("xmotion")
    e.xmotion.readField("x")
    e.xmotion.readField("y")
    val xFraction = e.xmotion.x.toFloat / width.toFloat
    val yFraction = 1f - e.xmotion.y.toFloat / height.toFloat
    InputEvent.Point(System.currentTimeMillis, xFraction, yFraction)
  }

  /**Captures events without propagation */
  case class Capture(evts: List[String])
      extends EventRecogniser
      with LazyLogging {
    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonPress =>
          Capture("ButtonPress" :: evts) -> Option.empty
        case MotionNotify =>
          Capture("MotionNotify" :: evts) -> Option.empty
        case ButtonRelease =>
          Capture("ButtonRelease" :: evts) -> Option.empty
        case LeaveNotify =>
          Capture("LeaveNotify" :: evts) -> Option.empty
        case other =>
          logger.warn("Unhandled event of type {}", other)
          this -> Option.empty
      }
    override def toString: String =
      s"Capture(captured: ${evts.reverse})"
  }

  case object Blank extends EventRecogniser with LazyLogging {
    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonPress =>
          logger.info("Blank: detected button press")
          val point = buttonEvent(e, width, height)
          MouseDown(point) -> Option.empty
        case MotionNotify =>
          this -> Option.empty
        case ButtonRelease =>
          this -> Option.empty
        case LeaveNotify =>
          this -> Option.empty
        case other =>
          logger.warn("Blank: Unhandled event of type {}", other)
          this -> Option.empty
      }
  }

  case class MouseDown(point: InputEvent.Point)
      extends EventRecogniser
      with LazyLogging {
    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonRelease =>
          val tap = InputEvent.Tap(point)
          logger.info(s"MouseDown: detected tap: $tap")
          Blank -> Some(tap)
        case MotionNotify =>
          logger.info("MouseDown: detected motionNotify")
          val current = motionEvent(e, width, height)
          DragContinuing(current :: List(point)) -> Some(
              InputEvent.DragStarted(point, current))
        case LeaveNotify =>
          logger.warn("MouseDown: detected leaveNotity")
          Blank -> Option.empty
        case other =>
          logger.warn("MouseDown: Unhandled event of type {}", other)
          this -> Option.empty
      }
  }

  case class DragContinuing(points: List[InputEvent.Point])
      extends EventRecogniser
      with LazyLogging {

    def start: InputEvent.Point = points.toList.last

    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonRelease =>
          val end = buttonEvent(e, width, height)
          if (InputEvent.distance(start, end) < 0.1) {
            logger.info("DragContinuing: detected tap")
            Blank -> Some(InputEvent.Tap(start))
          } else if (end.at - start.at < 1000L) {
            logger.info("DragContinuing: detected swipe")
            Blank -> Some(InputEvent.DragBecameSwipe(end :: points))
          } else {
            logger.info(s"DragContinuing: detected drag finish")
            Blank -> Some(InputEvent.DragFinished(end :: points))
          }
        case MotionNotify =>
          logger.debug("DragContinuing: detected drag")
          val end = motionEvent(e, width, height)
          if((end.at - points.head.at) > minDt) 
            DragContinuing(end :: points) -> Some(InputEvent.DragContinued(end :: points))
          else this -> Option.empty
        case LeaveNotify =>
          logger.warn("DragContinuing: detected leaveNotify")
          Blank -> Option.empty
        case ButtonPress =>
          logger.warn("DragContinuing: detected button press")
          Blank -> Option.empty
        case other =>
          logger.warn("DragContinuing: Unhandled event of type {}", other)
          this -> Option.empty
      }
  }
}
#-x11

