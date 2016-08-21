package iliad
package gfx

import iliad.{gl => GL}

import cats._
import cats.data._
import cats.free._
import cats.implicits._

import CatsExtra._

object ToGL {

  type DSL[A] = Free[ToGL, A]
  type Effect[A] = Reader[Graph.Constructed, A]

  def run[A](dsl: DSL[A]): Effect[A] = dsl.foldMap(ToGLInterpreter)

  def apply(t: Texture.Instance): DSL[GL.Texture.Constructor] =
    ToGLTexture(t).free

  def apply(r: Renderbuffer.Instance): DSL[GL.Renderbuffer.Constructor] =
    ToGLRenderbuffer(r).free

  private def transform(i: Framebuffer.OutputInstance)
    : DSL[GL.Framebuffer.AttachmentConstructor] =
    i match {
      case t: Texture.Instance => transform(t)
      case r: Renderbuffer.Instance => transform(r)
    }

  def apply(f: Framebuffer.Instance): DSL[GL.Framebuffer.Constructor] =
    f match {
      case Framebuffer.OnScreen => ToGLOnScreenFramebuffer.free
      case ff @ Framebuffer.OffScreenInstance(is) =>
        is.traverse { case (c, a) => transform(a).map(c -> _) }
          .flatMap(ToGLOffScreenFramebuffer(_).free)
    }

  private def apply(ts: Map[String, Texture.Uniform])
    : DSL[Map[String, GL.Texture.Constructor]] =
    ts.mapValues {
      case t: Texture.Instance => apply(t)
      case i: Texture.Image => ToGLImage(i).free
    }.sequence

  private def apply(n: Draw.Drawable): DSL[GL.DrawOp] =
    for {
      f <- apply(n.instance.framebuffer)
      tus <- apply(n.instance.textureUniforms)
    } yield
      GL.DrawOp(n.instance.model.model,
                n.instance.constructor.program,
                tus,
                n.uniforms,
                f,
                n.instance.constructor.colorMask,
                n.instance.constructor.primitive,
                n.instance.constructor.capabilities,
                n.instance.constructor.blend,
                n.instance.numInstances)

 private def apply(c: Clear.Instance): DSL[GL.ClearOp] =
    for {
      f <- apply(c.framebuffer)
} yield GL.ClearOp(c.constructor.mask, c.constructor.colour, f)

  def apply(
      ns: List[Node.Drawable]): DSL[List[XorT[GL.GL.DSL, GL.GLError, Unit]]] =
    ns.traverse {
      case d: Draw.Drawable =>
        apply(d).map(o => XorT(GL.GL.draw(o)))
      case c: Clear.Instance =>
        apply(c).map(o => XorT(GL.GL.clear(o)))
    }
}

sealed trait ToGL[A]

private case class ToGLTexture(t: Texture.Instance)
    extends ToGL[GL.Texture.Constructor]
private case class ToGLImage(i: Texture.Image)
    extends ToGL[GL.Texture.Constructor]
private case class ToGLRenderbuffer(r: Renderbuffer.Instance)
    extends ToGL[GL.Renderbuffer.Constructor]
private case object ToGLOnScreenFramebuffer
    extends ToGL[GL.Framebuffer.Constructor]
private case class ToGLOffScreenFramebuffer(
    as: List[(GL.FramebufferAttachment, GL.Framebuffer.AttachmentConstructor)])
    extends ToGL[GL.Framebuffer.Constructor]

object ToGLInterpreter extends (ToGL ~> ToGL.Effect) {
  def apply[A](t: ToGL[A]): ToGL.Effect[A] = t match {
    case ToGLTexture(t) =>
      Reader { c =>
        val tt: GL.Texture.Constructor =
          if (c.doubleTextures.contains(t.constructor)) {
            GL.Texture.DoubleConstructor(s"${t.name}-${t.constructor.name}",
                                         t.constructor.format,
                                         t.constructor.viewport)
          } else {
            GL.Texture.SingleConstructor(s"${t.name}-${t.constructor.name}",
                                         t.constructor.format,
                                         t.constructor.viewport)
          }
        tt
      }
    case ToGLImage(i) =>
      Kleisli.pure(GL.Texture.SingleConstructor(i.name, i.format, i.viewport))
    case ToGLRenderbuffer(r) =>
      Kleisli.pure(
          GL.Renderbuffer.Constructor(s"${r.name}-${r.constructor.name}",
                                      r.constructor.format,
                                      r.constructor.viewport))
    case ToGLOnScreenFramebuffer => Kleisli.pure(GL.Framebuffer.default)
    case ToGLOffScreenFramebuffer(as) =>
      val isDouble = as.exists {
        case (_, t: GL.Texture.DoubleConstructor) => true
        case _ => false
      }
      if (isDouble) Kleisli.pure(GL.Framebuffer.DoubleConstructor(as))
      else Kleisli.pure(GL.Framebuffer.SingleConstructor(as))
  }
}
