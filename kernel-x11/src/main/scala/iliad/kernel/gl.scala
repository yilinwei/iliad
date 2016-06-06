package iliad
package kernel

import iliad.kernel.platform.{GLES30Library => Lib}
import iliad.kernel.platform.unix._

@jna[GLES30Library] trait GLES30Binding

object GLES30 extends Lib with GLES30Binding