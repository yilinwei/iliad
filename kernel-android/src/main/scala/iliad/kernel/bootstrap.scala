package iliad.kernel

import scala.reflect._

import iliad.kernel._

import android.app.Activity
import android.app.Fragment
import android.os.Bundle
import android.util.Log
import android.view._
import android.graphics.Point
import android.support.v4.view.GestureDetectorCompat
import android.content.Context

import fs2._
import fs2.util._
import fs2.async.mutable._

import com.typesafe.scalalogging._

import cats._
import cats.data._
import cats.implicits._

trait AndroidDependencies extends GLDependencies with IliadApp {

  type NativeDisplay = iliad.kernel.EGL14.EGLNativeDisplayType
  type NativeWindow = iliad.kernel.EGL14.EGLNativeWindowType
  type NativePixmap = iliad.kernel.EGL14.EGLNativePixmapType
  type EGLConfig = iliad.kernel.EGL14.EGLConfig
  type EGLSurface = iliad.kernel.EGL14.EGLSurface
  type EGLDisplay = iliad.kernel.EGL14.EGLDisplay
  type EGLContext = iliad.kernel.EGL14.EGLContext

  val configClassTag = classTag[iliad.kernel.EGL14.EGLConfig]

  val EGL14 = iliad.kernel.EGL14
  val GLES30 = iliad.kernel.GLES30
}

trait AndroidVSync {

  private implicit val S: Strategy = Strategy.fromFixedDaemonPool(1, "vsync")

  private val _choreographer = Choreographer.getInstance

  private def vsync(s: Signal[Task, Long]): Unit =
    _choreographer.postFrameCallback {
      new Choreographer.FrameCallback() {
        def doFrame(frameTimeNanos: Long): Unit = {
          s.set(frameTimeNanos).unsafeAttemptRun.toXor match {
            case Xor.Right(_) =>
            case Xor.Left(err) =>
              throw new Error(
                s"Failed to set vsync signal at time $frameTimeNanos. ${err.getMessage}"
              )
          }
          _choreographer.postFrameCallback(this)
        }
      }
    }

  def vsync: Stream[Task, Long] =
    Stream.eval(async.signalOf[Task, Long](0L)).flatMap { s =>
      vsync(s)
      s.discrete
    }
}

trait AndroidBootstrap extends Activity with AndroidEventHandler
    with AndroidDependencies with AndroidVSync with LazyLogging {
  app: IliadApp =>

  val pageSize: Int = 1024

  def mainXML: Int
  def fragmentXML: Int
  def subFragment: Int
  def subView: Int

  var detector: GestureDetectorCompat = _

  var _width: Int = _
  var _height: Int = _
  def width: Int = _width
  def height: Int = _height

  override def onCreate(savedInstanceState: Bundle) {
    logger.info("AndroidBootstrap.onCreate: creating activity")
    super.onCreate(savedInstanceState)
    setContentView(mainXML)

    val wm: WindowManager = getSystemService(Context.WINDOW_SERVICE).asInstanceOf[WindowManager];
    val display = wm.getDefaultDisplay();
    val size = new Point()
    display.getSize(size)
    _width = size.x
    _height = size.y

    detector = new GestureDetectorCompat(this, this)
    detector.setOnDoubleTapListener(this)

    if (savedInstanceState == null) {
      val transaction = getFragmentManager.beginTransaction()
      val fragment = new AndroidFragment(app, subView, fragmentXML, session)
      transaction.replace(subFragment, fragment)
      transaction.commit()
    }
  }

  override def onTouchEvent(event: MotionEvent): Boolean = {
    detector.onTouchEvent(event)      
    super.onTouchEvent(event)
  }

  override def onCreateOptionsMenu(menu: Menu): Boolean = true
  override def onPrepareOptionsMenu(menu: Menu): Boolean = true
  override def onOptionsItemSelected(item: MenuItem): Boolean = true
}

final class AndroidFragment(app: IliadApp, subView: Int, fragmentXML: Int,
  session: BlockingPromise[(SurfaceHolder, Int)]) extends Fragment with LazyLogging {

  override def onCreateView(inflater: LayoutInflater ,container: ViewGroup, savedInstanceState: Bundle) = {
    logger.info("AndroidFragment.onCreateView: creating view")
    inflater.inflate(fragmentXML, container, false)
  }

  override def onViewCreated(v: View, savedInstanceState: Bundle): Unit = {
    logger.info("AndroidFragment.onViewCreated: created view. Running app")
    val view = v.findViewById(subView).asInstanceOf[SurfaceView]
    session.set((view.getHolder(), EGL14.EGL_DEFAULT_DISPLAY))
    app.run()
  }
}
