package iliad
package gfx

import iliad.gl._

import cats._
import cats.data._
import cats.implicits._

import CatsExtra._

object Load {
  type Effect = Reader[Graphics.Config, XorT[GL.DSL, GLError, Unit]]

  private def lift[A](dsl: GL.DSL[A]): Effect =
    Kleisli.pure(XorT.right[GL.DSL, GLError, Unit](dsl.map(_ => ())))

  private def lift[A](f: Graphics.Config => GL.DSL[A]): Effect =
    Reader(cfg => XorT.right[GL.DSL, GLError, Unit](f(cfg).map(_ => ())))

  private[gfx] def apply(l: Load): Effect = l match {
    case PutProgram(p) => lift(GL.load(p))
    case PutVertices(r, d) => lift(cfg => GL.load(r, d, cfg.pageSize))
    case PutElements(r, d) => lift(cfg => GL.load(r, d, cfg.pageSize))
    case PutTexture(t, d) =>
      lift(cfg => GL.load(ToGL.run(ToGL(t)).run(cfg.graph), d))
    case PutRenderbuffer(r) =>
      lift(cfg => GL.load(ToGL.run(ToGL(r)).run(cfg.graph)))
    case PutFramebuffer(f) =>
      Reader(cfg =>
            XorT(GL.load(ToGL.run(ToGL(f)).run(cfg.graph))).leftWiden[GLError])
  }
}

sealed trait Load

private case class PutProgram(p: Program.Unlinked) extends Load
private case class PutVertices(r: VertexData.Ref, d: VertexData.Data)
    extends Load
private case class PutElements(r: ElementData.Ref, d: ElementData.Data)
    extends Load
private case class PutTexture(t: Texture.Instance, d: Option[gl.Texture.Data])
    extends Load
private case class PutRenderbuffer(r: Renderbuffer.Instance) extends Load
private case class PutFramebuffer(f: Framebuffer.Instance) extends Load

trait LoadFunctions {

  private def lift(l: Load): Graphics.Graphics =
    shapeless.Coproduct[Graphics.Graphics](l)

  def load(
      vf: (VertexShader.Source, FragmentShader.Source)): Graphics.Graphics = {
    val (v, f) = vf
    lift(PutProgram(Program.Unlinked(v, f)))
  }

  def load(r: VertexData.Ref, d: VertexData.Data): Graphics.Graphics =
    lift(PutVertices(r, d))

  def load(r: ElementData.Ref, d: ElementData.Data): Graphics.Graphics =
    lift(PutElements(r, d))

  def load(t: Texture.Instance,
           d: Option[gl.Texture.Data]): Graphics.Graphics =
    lift(PutTexture(t, d))

  def load(r: Renderbuffer.Instance): Graphics.Graphics =
    lift(PutRenderbuffer(r))

  def load(f: Framebuffer.Instance): Graphics.Graphics =
    lift(PutFramebuffer(f))
}
