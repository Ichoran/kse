// This file is distributed under the BSD 3-clause license.  See file LICENSE.
// Copyright (c) 2016 Rex Kerr

package kse.visual

import java.io._
import java.awt.image._

import kse.flow._
import kse.coll._
import kse.eio._


class Bitmap(val packed: Array[Int], val offset: Int, val stride: Int, val w: Int, val h: Int, val hasAlpha: Boolean) {
  def apply(x: Int, y: Int) = packed(offset + x + stride*y)
  def rgba(x: Int, y: Int) = { val i = apply(x,y); if (hasAlpha) Rgba.bgra(i) else Rgba.bgr(i) }
  def update(x: Int, y: Int, value: Int) {
    packed(offset + x + stride*y) = value
  }
  def update(x: Int, y: Int, value: Rgba) {
    packed(offset + x + stride*y) = value.bgraInt
  }

  def fill(f: (Int, Int) => Int) {
    var y = 0
    while (y < h) {
      var x = 0
      var i = offset + stride*y
      while (x < w) {
        packed(i) = f(x, y)
        i += 1
        x += 1
      }
      y += 1
    }
  }
  def fill[R <: Rgba](f: (Int, Int) => Rgba)(implicit ev: R =:= Rgba) {
    var y = 0
    while (y < h) {
      var x = 0
      var i = offset + stride*y
      while (x < w) {
        packed(i) = f(x, y).bgraInt
        i += 1
        x += 1
      }
      y += 1
    }
  }

  def size = w*h

  def scale(level: Int) =
    if (level < -1) {
      val mult = -level
      val nw = w*mult
      val nh = w*mult
      val data = new Array[Int](nw * nh)
      var ny = 0
      var oy = 0
      while (oy < h) {
        var nx = 0
        var ox = 0
        while (ox < w) {
          val i = packed(oy*stride + offset+ ox)
          var y = 0
          while (y < mult) {
            val yidx = (ny + y)*nw
            var x = 0
            while (x < mult) {
              data(yidx + nx + x) = i
              x += 1
            }
            y += 1
          }
          nx += mult
          ox += 1
        }
        ny += mult
        oy += 1
      }
      new Bitmap(data, 0, nw, nw, nh, hasAlpha)
    }
    else if (level > 1) {
      val nw = w/level
      val nh = h/level
      val data = new Array[Int](nw * nh)
      var ny = 0
      var oy = level/2
      while (ny < nh) {
        var nx = 0
        var ox = level/2
        while (nx < nw) {
          data(ny*nw + nx) = packed(oy*stride + offset + ox)
          ox += level
          nx += 1
        }
        oy += level
        ny += 1
      }
      new Bitmap(data, 0, nw, nw, nh, hasAlpha)
    }
    else deepCopy

  def binAsUInt(binning: Int): Bitmap = {
    if (binning < 2) throw new IllegalArgumentException("Binning should be at least 2, not "+binning)
    val nw = w/binning
    val nh = h/binning
    val bsq = binning*binning
    val data = new Array[Int](nw * nh)
    var ny = 0
    while (ny < nh) {
      var nx = 0
      while (nx < nw) {
        var sum = 0L
        var idx = stride*ny*binning + offset + nx*binning
        var y = 0
        while (y < binning) {
          var x = 0
          while (x < binning) {
            sum += packed(idx + stride*y + x) & 0xFFFFFFFFL
            x += 1
          }
          y += 1
        }
        data(ny*nw + nx) = (sum/bsq).toInt
        nx += 1
      }
      ny += 1
    }
    new Bitmap(data, 0, nw, nw, nh, hasAlpha)
  }


  def deepCopy: Bitmap = new Bitmap(java.util.Arrays.copyOf(packed, packed.length), offset, stride, w, h, hasAlpha)
  def trim = 
    if (packed.length == w*h) this
    else if (stride == w) new Bitmap(java.util.Arrays.copyOfRange(packed, offset, offset + w*h), 0, w, w, h, hasAlpha)
    else {
      val a = new Array[Int](w*h)
      var i = offset
      var j = 0
      var y = 0
      if (w >= 16) {
        while (y < h) {
          java.lang.System.arraycopy(packed, i, a, j, w)
          i += stride
          j += w
          y += 1
        }
      }
      else {
        while (y < h) {
          val ii = i + stride
          val jj = j + w
          while (j < jj) { a(j) = packed(i); i += 1; j += 1 }
          i = ii
          y += 1
        }
      }
      new Bitmap(a, 0, w, w, h, hasAlpha)
    }
  def crop(x: Int, y: Int, nx: Int, ny: Int) = {
    val kx = math.max(0, math.min(w-x, nx))
    val ky = math.max(0, math.min(h-y, ny))
    val cx = math.max(0, math.min(x, w))
    val cy = math.max(0, math.min(y, h))
    new Bitmap(packed, offset + stride*cy + cx, stride, ky, kx, hasAlpha)
  }

  def pasteInto(target: Bitmap, x: Int, y: Int) {
    if (x >= 0 && x < target.w && y >= 0 && y < target.h) {
      if (x == 0 && stride == w && target.stride == target.w && w == target.w && hasAlpha == target.hasAlpha)
        java.lang.System.arraycopy(packed, offset, target.packed, target.offset + target.stride*y, w*math.min(h, target.h-y))
      else {
        val nx = math.min(w, target.w - x)
        var ny = math.min(h, target.h - y)
        var i = offset
        var j = target.offset + target.stride*y + x
        if (nx >= 16 && hasAlpha == target.hasAlpha) {
          while (ny > 0) {
            java.lang.System.arraycopy(packed, i, target.packed, j, nx)
            i += stride
            j += target.stride
            ny -= 1
          }
        }
        else {
          while (ny > 0) {
            val ii = i + stride
            val jj = j + target.stride
            var n = nx
            if (hasAlpha == target.hasAlpha) {
              while (n > 0) {
                target.packed(j) = packed(i)
                i += 1
                j += 1
                n -= 1
              }
            }
            else {
              while (n > 0) {
                target.packed(j) = 0xFF000000 | packed(i)
                i += 1
                j += 1
                n -= 1
              }
            }
            i = ii
            j = jj
            ny -= 1
          }
        }
      }
    }
  }
  def pasteInto(target: BufferedImage, x: Int, y: Int) {
    if (hasAlpha) target.setRGB(x, y, w, h, packed, offset, stride)
    else {
      var j = 0
      while (j < h) {
        var i = 0
        while (i < w) {
          target.setRGB(x + i, y + j, packed(offset + stride*j + i) | 0xFF000000)
          i += 1
        }
        j += 1
      }
    }
  }

  def toImage: BufferedImage = {
    val cm = new DirectColorModel(32, 0x00FF0000, 0x0000FF00, 0x000000FF, 0xFF000000)
    val wr = cm.createCompatibleWritableRaster(w, h)
    val bi = new BufferedImage(cm, wr, false, null)
    pasteInto(bi, 0, 0)
    bi
  }

  def toFile(f: File): Ok[String, Unit] =
    safe{ javax.imageio.ImageIO.write(toImage, "png", f) }.
      mapNo(e => f"Could not write to ${f.getPath}\n${e.toString}").
      flatMap{
        case false => No(f"PNG writer not installed?!")
        case true => Ok.UnitYes
      }

  def toDataURI = {
    val baos = new ByteArrayOutputStream
    javax.imageio.ImageIO.write(toImage, "png", baos)
    Bitmap.dataURIPrefix + base64.DataURI.encode(baos.toByteArray)
  }
}

object Bitmap{
  final val dataURIPrefix = "data:image/png;base64,"
  def apply(im: BufferedImage): Bitmap = {
    val h = im.getHeight
    val w = im.getWidth
    val bm = new Bitmap(new Array[Int](h*w), 0, w, w, h, im.getColorModel.hasAlpha)
    im.getRGB(0, 0, w, h, bm.packed, 0, w)
    bm
  }

  def empty(w: Int, h: Int): Bitmap = {
    new Bitmap(new Array[Int](w*h), 0, w, w, h, true)
  }

  def fill(w: Int, h: Int)(f: (Int, Int) => Int): Bitmap = { val bm = Bitmap.empty(w,h); bm.fill(f); bm }
  def fillRgba(w: Int, h: Int)(f: (Int, Int) => Rgba): Bitmap = { val bm = Bitmap.empty(w,h); bm.fill(f); bm }

  def opaque(w: Int, h: Int): Bitmap = {
    new Bitmap(new Array[Int](w*h), 0, w, w, h, false)
  }

  def from(f: File): Ok[String, Bitmap] =
    safe{ apply(javax.imageio.ImageIO.read(f)) }.
      mapNo(e => f"Could not read image from ${f.getPath}\n${e.toString}")

  def fromDataURI(data: String): Ok[String, Bitmap] =
    if (!data.startsWith(dataURIPrefix)) No("Does not start with "+dataURIPrefix+"; starts with "+data.take(dataURIPrefix.length))
    else probably{ implicit oops => base64.DataURI.decode(data.drop(dataURIPrefix.length)) } match {
      case Some(bytes) =>
        val bais = new ByteArrayInputStream(bytes)
        safe{ javax.imageio.ImageIO.read(bais) } match {
          case Yes(png) => Yes(apply(png))
          case No(e) => No("Exception while converting to PNG: " + e.toString)
        }
      case _ => No("Malformed Base64")
    }
}
