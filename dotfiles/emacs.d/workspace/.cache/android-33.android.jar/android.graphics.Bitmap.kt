package android.graphics

import android.annotation.NonNull
import android.annotation.Nullable
import android.graphics.Bitmap.CompressFormat
import android.graphics.Bitmap.Config
import android.hardware.HardwareBuffer
import android.os.Parcel
import android.os.Parcelable
import android.os.Parcelable.Creator
import android.util.DisplayMetrics
import java.io.OutputStream
import java.nio.Buffer

class Bitmap : Parcelable {
    companion object {
        @JvmStatic val CREATOR: Creator = null
        @JvmStatic val DENSITY_NONE: Int = 0
        @JvmStatic fun wrapHardwareBuffer(hardwareBuffer: HardwareBuffer, colorSpace: ColorSpace): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createScaledBitmap(src: Bitmap, dstWidth: Int, dstHeight: Int, filter: Boolean): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(src: Bitmap): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(source: Bitmap, x: Int, y: Int, width: Int, height: Int): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(source: Bitmap, x: Int, y: Int, width: Int, height: Int, m: Matrix, filter: Boolean): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(width: Int, height: Int, config: Config): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(display: DisplayMetrics, width: Int, height: Int, config: Config): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(width: Int, height: Int, config: Config, hasAlpha: Boolean): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(width: Int, height: Int, config: Config, hasAlpha: Boolean, colorSpace: ColorSpace): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(display: DisplayMetrics, width: Int, height: Int, config: Config, hasAlpha: Boolean): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(display: DisplayMetrics, width: Int, height: Int, config: Config, hasAlpha: Boolean, colorSpace: ColorSpace): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(colors: IntArray, offset: Int, stride: Int, width: Int, height: Int, config: Config): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(display: DisplayMetrics, colors: IntArray, offset: Int, stride: Int, width: Int, height: Int, config: Config): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(colors: IntArray, width: Int, height: Int, config: Config): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(display: DisplayMetrics, colors: IntArray, width: Int, height: Int, config: Config): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(source: Picture): Bitmap {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createBitmap(source: Picture, width: Int, height: Int, config: Config): Bitmap {
            throw RuntimeException("Stub!")
        }
    }

    fun Bitmap() {
        throw RuntimeException("Stub!")
    }

    fun getDensity(): Int {
        throw RuntimeException("Stub!")
    }

    fun setDensity(density: Int) {
        throw RuntimeException("Stub!")
    }

    fun reconfigure(width: Int, height: Int, config: Config) {
        throw RuntimeException("Stub!")
    }

    fun setWidth(width: Int) {
        throw RuntimeException("Stub!")
    }

    fun setHeight(height: Int) {
        throw RuntimeException("Stub!")
    }

    fun setConfig(config: Config) {
        throw RuntimeException("Stub!")
    }

    fun recycle() {
        throw RuntimeException("Stub!")
    }

    fun isRecycled(): Boolean {
        throw RuntimeException("Stub!")
    }

    fun getGenerationId(): Int {
        throw RuntimeException("Stub!")
    }

    fun copyPixelsToBuffer(dst: Buffer) {
        throw RuntimeException("Stub!")
    }

    fun copyPixelsFromBuffer(src: Buffer) {
        throw RuntimeException("Stub!")
    }

    fun copy(config: Config, isMutable: Boolean): Bitmap {
        throw RuntimeException("Stub!")
    }

    fun asShared(): Bitmap {
        throw RuntimeException("Stub!")
    }

    fun getNinePatchChunk(): ByteArray {
        throw RuntimeException("Stub!")
    }

    fun compress(format: CompressFormat, quality: Int, stream: OutputStream): Boolean {
        throw RuntimeException("Stub!")
    }

    fun isMutable(): Boolean {
        throw RuntimeException("Stub!")
    }

    fun isPremultiplied(): Boolean {
        throw RuntimeException("Stub!")
    }

    fun setPremultiplied(premultiplied: Boolean) {
        throw RuntimeException("Stub!")
    }

    fun getWidth(): Int {
        throw RuntimeException("Stub!")
    }

    fun getHeight(): Int {
        throw RuntimeException("Stub!")
    }

    fun getScaledWidth(canvas: Canvas): Int {
        throw RuntimeException("Stub!")
    }

    fun getScaledHeight(canvas: Canvas): Int {
        throw RuntimeException("Stub!")
    }

    fun getScaledWidth(metrics: DisplayMetrics): Int {
        throw RuntimeException("Stub!")
    }

    fun getScaledHeight(metrics: DisplayMetrics): Int {
        throw RuntimeException("Stub!")
    }

    fun getScaledWidth(targetDensity: Int): Int {
        throw RuntimeException("Stub!")
    }

    fun getScaledHeight(targetDensity: Int): Int {
        throw RuntimeException("Stub!")
    }

    fun getRowBytes(): Int {
        throw RuntimeException("Stub!")
    }

    fun getByteCount(): Int {
        throw RuntimeException("Stub!")
    }

    fun getAllocationByteCount(): Int {
        throw RuntimeException("Stub!")
    }

    fun getConfig(): Config {
        throw RuntimeException("Stub!")
    }

    fun hasAlpha(): Boolean {
        throw RuntimeException("Stub!")
    }

    fun setHasAlpha(hasAlpha: Boolean) {
        throw RuntimeException("Stub!")
    }

    fun hasMipMap(): Boolean {
        throw RuntimeException("Stub!")
    }

    fun setHasMipMap(hasMipMap: Boolean) {
        throw RuntimeException("Stub!")
    }

    fun getColorSpace(): ColorSpace {
        throw RuntimeException("Stub!")
    }

    fun setColorSpace(colorSpace: ColorSpace) {
        throw RuntimeException("Stub!")
    }

    fun eraseColor(c: Int) {
        throw RuntimeException("Stub!")
    }

    fun eraseColor(color: Long) {
        throw RuntimeException("Stub!")
    }

    fun getPixel(x: Int, y: Int): Int {
        throw RuntimeException("Stub!")
    }

    fun getColor(x: Int, y: Int): Color {
        throw RuntimeException("Stub!")
    }

    fun getPixels(pixels: IntArray, offset: Int, stride: Int, x: Int, y: Int, width: Int, height: Int) {
        throw RuntimeException("Stub!")
    }

    fun setPixel(x: Int, y: Int, color: Int) {
        throw RuntimeException("Stub!")
    }

    fun setPixels(pixels: IntArray, offset: Int, stride: Int, x: Int, y: Int, width: Int, height: Int) {
        throw RuntimeException("Stub!")
    }

    fun describeContents(): Int {
        throw RuntimeException("Stub!")
    }

    fun writeToParcel(p: Parcel, flags: Int) {
        throw RuntimeException("Stub!")
    }

    fun extractAlpha(): Bitmap {
        throw RuntimeException("Stub!")
    }

    fun extractAlpha(paint: Paint, offsetXY: IntArray): Bitmap {
        throw RuntimeException("Stub!")
    }

    fun sameAs(other: Bitmap): Boolean {
        throw RuntimeException("Stub!")
    }

    fun prepareToDraw() {
        throw RuntimeException("Stub!")
    }

    fun getHardwareBuffer(): HardwareBuffer {
        throw RuntimeException("Stub!")
    }
}