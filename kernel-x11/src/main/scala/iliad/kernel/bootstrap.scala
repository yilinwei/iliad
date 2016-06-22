package iliad
package kernel

import scala.reflect._

import iliad.kernel.platform.unix.X11

import iliad.kernel._

import com.sun.jna.platform.unix.X11._
import com.sun.jna._

import cats._
import cats.data._
import cats.implicits._

import org.slf4j._

trait X11GLDependencies extends GLDependencies with IliadApp {

  type NativeDisplay = iliad.kernel.EGL14.EGLNativeDisplayType
  type NativeWindow = iliad.kernel.EGL14.EGLNativeWindowType
  type NativePixmap = iliad.kernel.EGL14.EGLNativePixmapType
  type EGLConfig = iliad.kernel.EGL14.EGLConfig
  type EGLSurface = iliad.kernel.EGL14.EGLSurface
  type EGLDisplay = iliad.kernel.EGL14.EGLDisplay
  type EGLContext = iliad.kernel.EGL14.EGLContext

  val configClassTag = classTag[iliad.kernel.platform.unix.EGLConfig]

  val EGL14 = iliad.kernel.EGL14
  val GLES30 = iliad.kernel.GLES30
}

trait X11Bootstrap extends X11EventHandler with X11GLDependencies {

  private val log = LoggerFactory.getLogger(classOf[X11Bootstrap])

  def width: Int
  def height: Int

  private val x = iliad.kernel.platform.unix.X11.INSTANCE

  override val lockDisplay = Some(x.XLockDisplay _)
  override val unlockDisplay = Some(x.XUnlockDisplay _)

  private def initThreads(): Error Xor Unit = {
    val code = x.XInitThreads()
    if (code == 0) new Error("Failed to multi thread X11").left
    else ().right
  }

  private def openDisplay(): Error Xor Display =
    Option(x.XOpenDisplay(null)) match {
      case Some(d) => d.right
      case scala.None => new Error("Failed to open display").left
    }

  private def rootWindow(d: Display): Error Xor Window =
    Option(x.XRootWindow(d, x.XDefaultScreen(d))) match {
      case Some(w) => w.right
      case scala.None => new Error("Failed to find root window").left
    }

  private def createSimpleWindow(d: Display, root: Window): Error Xor Window = {
    val xOffset = 0
    val yOffset = 0
    val borderWidth = 1
    val border = 1
    val background = 0
    log.debug("Creating window with width {} height {}",
              width,
              height)
    try {
      x.XCreateSimpleWindow(
            d,
            root,
            xOffset,
            yOffset,
            width,
            height,
            borderWidth,
            border,
            background
        )
        .right
    } catch {
      case e: Error =>
        new Error(s"Failed to create window: \n ${e.getMessage}").left
    }
  }

  private def addDeletionProtocol(d: Display, w: Window): Error Xor Unit = {
    log.debug("Adding deletion protocol")
    val protocol = x.XInternAtom(d, "WM_DELETE_WINDOW", false)
    x.XSetWMProtocols(d, w, Array(protocol), 1) match {
      case 0 => new Error("Unable to set deletion protocol").left
      case _ => ().right
    }
  }

  private val inputMask = new NativeLong(ExposureMask | ButtonPressMask)

  private def addInputDetection(d: Display, w: Window): Unit = {
    log.debug("Adding input detection")
    x.XSelectInput(d, w, inputMask)
  }

  private def showWindow(d: Display, w: Window): Unit = {
    log.debug("Showing window")
    x.XMapWindow(d, w)
  }

  private def createWindow: Error Xor (Display, Window) =
    for {
      _ <- initThreads()
      d <- openDisplay()
      r <- rootWindow(d)
      w <- createSimpleWindow(d, r)
      _ <- addDeletionProtocol(d, w)
      _ = addInputDetection(d, w)
      _ = showWindow(d, w)
    } yield (d, w)

  private def destroyWindow(d: Display, w: Window): Unit = {
    x.XDestroyWindow(d, w)
    x.XCloseDisplay(d)
  }

  private def handleAllEvents(d: Display): Boolean = {
    val e = new XEvent()
    x.XNextEvent(d, e)
    e.`type` match {
      case ClientMessage =>
        log.info("Closing window")
        false
      case other =>
        handleEvent(e)
        true
    }
  }

  def main(args: Array[String]): Unit = {
    println("running app")
    createWindow match {
      case Xor.Right((d, w)) =>
        log.info("Created window")
        session.set((w, d))
        run()

        var shouldDraw = true
        while (shouldDraw) {
          x.XLockDisplay(d)
          shouldDraw = handleAllEvents(d)
          x.XUnlockDisplay(d)
        }
        destroyWindow(d, w)
      case Xor.Left(err) =>
        session.set(err)
    }
  }
}
