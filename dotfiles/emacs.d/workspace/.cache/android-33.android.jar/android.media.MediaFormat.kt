package android.media

import android.annotation.NonNull
import android.annotation.Nullable
import java.nio.ByteBuffer

class MediaFormat {
    companion object {
        @JvmStatic val COLOR_RANGE_FULL: Int = 1
        @JvmStatic val COLOR_RANGE_LIMITED: Int = 2
        @JvmStatic val COLOR_STANDARD_BT2020: Int = 6
        @JvmStatic val COLOR_STANDARD_BT601_NTSC: Int = 4
        @JvmStatic val COLOR_STANDARD_BT601_PAL: Int = 2
        @JvmStatic val COLOR_STANDARD_BT709: Int = 1
        @JvmStatic val COLOR_TRANSFER_HLG: Int = 7
        @JvmStatic val COLOR_TRANSFER_LINEAR: Int = 1
        @JvmStatic val COLOR_TRANSFER_SDR_VIDEO: Int = 3
        @JvmStatic val COLOR_TRANSFER_ST2084: Int = 6
        @JvmStatic val KEY_AAC_DRC_ALBUM_MODE: String = "aac-drc-album-mode"
        @JvmStatic val KEY_AAC_DRC_ATTENUATION_FACTOR: String = "aac-drc-cut-level"
        @JvmStatic val KEY_AAC_DRC_BOOST_FACTOR: String = "aac-drc-boost-level"
        @JvmStatic val KEY_AAC_DRC_EFFECT_TYPE: String = "aac-drc-effect-type"
        @JvmStatic val KEY_AAC_DRC_HEAVY_COMPRESSION: String = "aac-drc-heavy-compression"
        @JvmStatic val KEY_AAC_DRC_OUTPUT_LOUDNESS: String = "aac-drc-output-loudness"
        @JvmStatic val KEY_AAC_DRC_TARGET_REFERENCE_LEVEL: String = "aac-target-ref-level"
        @JvmStatic val KEY_AAC_ENCODED_TARGET_LEVEL: String = "aac-encoded-target-level"
        @JvmStatic val KEY_AAC_MAX_OUTPUT_CHANNEL_COUNT: String = "aac-max-output-channel_count"
        @JvmStatic val KEY_AAC_PROFILE: String = "aac-profile"
        @JvmStatic val KEY_AAC_SBR_MODE: String = "aac-sbr-mode"
        @JvmStatic val KEY_ALLOW_FRAME_DROP: String = "allow-frame-drop"
        @JvmStatic val KEY_AUDIO_SESSION_ID: String = "audio-session-id"
        @JvmStatic val KEY_BITRATE_MODE: String = "bitrate-mode"
        @JvmStatic val KEY_BIT_RATE: String = "bitrate"
        @JvmStatic val KEY_CAPTION_SERVICE_NUMBER: String = "caption-service-number"
        @JvmStatic val KEY_CAPTURE_RATE: String = "capture-rate"
        @JvmStatic val KEY_CHANNEL_COUNT: String = "channel-count"
        @JvmStatic val KEY_CHANNEL_MASK: String = "channel-mask"
        @JvmStatic val KEY_CODECS_STRING: String = "codecs-string"
        @JvmStatic val KEY_COLOR_FORMAT: String = "color-format"
        @JvmStatic val KEY_COLOR_RANGE: String = "color-range"
        @JvmStatic val KEY_COLOR_STANDARD: String = "color-standard"
        @JvmStatic val KEY_COLOR_TRANSFER: String = "color-transfer"
        @JvmStatic val KEY_COLOR_TRANSFER_REQUEST: String = "color-transfer-request"
        @JvmStatic val KEY_COMPLEXITY: String = "complexity"
        @JvmStatic val KEY_CREATE_INPUT_SURFACE_SUSPENDED: String = "create-input-buffers-suspended"
        @JvmStatic val KEY_CROP_BOTTOM: String = "crop-bottom"
        @JvmStatic val KEY_CROP_LEFT: String = "crop-left"
        @JvmStatic val KEY_CROP_RIGHT: String = "crop-right"
        @JvmStatic val KEY_CROP_TOP: String = "crop-top"
        @JvmStatic val KEY_DURATION: String = "durationUs"
        @JvmStatic val KEY_ENCODER_DELAY: String = "encoder-delay"
        @JvmStatic val KEY_ENCODER_PADDING: String = "encoder-padding"
        @JvmStatic val KEY_FLAC_COMPRESSION_LEVEL: String = "flac-compression-level"
        @JvmStatic val KEY_FRAME_RATE: String = "frame-rate"
        @JvmStatic val KEY_GRID_COLUMNS: String = "grid-cols"
        @JvmStatic val KEY_GRID_ROWS: String = "grid-rows"
        @JvmStatic val KEY_HAPTIC_CHANNEL_COUNT: String = "haptic-channel-count"
        @JvmStatic val KEY_HARDWARE_AV_SYNC_ID: String = "hw-av-sync-id"
        @JvmStatic val KEY_HDR10_PLUS_INFO: String = "hdr10-plus-info"
        @JvmStatic val KEY_HDR_STATIC_INFO: String = "hdr-static-info"
        @JvmStatic val KEY_HEIGHT: String = "height"
        @JvmStatic val KEY_INTRA_REFRESH_PERIOD: String = "intra-refresh-period"
        @JvmStatic val KEY_IS_ADTS: String = "is-adts"
        @JvmStatic val KEY_IS_AUTOSELECT: String = "is-autoselect"
        @JvmStatic val KEY_IS_DEFAULT: String = "is-default"
        @JvmStatic val KEY_IS_FORCED_SUBTITLE: String = "is-forced-subtitle"
        @JvmStatic val KEY_I_FRAME_INTERVAL: String = "i-frame-interval"
        @JvmStatic val KEY_LANGUAGE: String = "language"
        @JvmStatic val KEY_LATENCY: String = "latency"
        @JvmStatic val KEY_LEVEL: String = "level"
        @JvmStatic val KEY_LOW_LATENCY: String = "low-latency"
        @JvmStatic val KEY_MAX_B_FRAMES: String = "max-bframes"
        @JvmStatic val KEY_MAX_FPS_TO_ENCODER: String = "max-fps-to-encoder"
        @JvmStatic val KEY_MAX_HEIGHT: String = "max-height"
        @JvmStatic val KEY_MAX_INPUT_SIZE: String = "max-input-size"
        @JvmStatic val KEY_MAX_OUTPUT_CHANNEL_COUNT: String = "max-output-channel-count"
        @JvmStatic val KEY_MAX_PTS_GAP_TO_ENCODER: String = "max-pts-gap-to-encoder"
        @JvmStatic val KEY_MAX_WIDTH: String = "max-width"
        @JvmStatic val KEY_MIME: String = "mime"
        @JvmStatic val KEY_MPEGH_COMPATIBLE_SETS: String = "mpegh-compatible-sets"
        @JvmStatic val KEY_MPEGH_PROFILE_LEVEL_INDICATION: String = "mpegh-profile-level-indication"
        @JvmStatic val KEY_MPEGH_REFERENCE_CHANNEL_LAYOUT: String = "mpegh-reference-channel-layout"
        @JvmStatic val KEY_OPERATING_RATE: String = "operating-rate"
        @JvmStatic val KEY_OUTPUT_REORDER_DEPTH: String = "output-reorder-depth"
        @JvmStatic val KEY_PCM_ENCODING: String = "pcm-encoding"
        @JvmStatic val KEY_PICTURE_TYPE: String = "picture-type"
        @JvmStatic val KEY_PIXEL_ASPECT_RATIO_HEIGHT: String = "sar-height"
        @JvmStatic val KEY_PIXEL_ASPECT_RATIO_WIDTH: String = "sar-width"
        @JvmStatic val KEY_PREPEND_HEADER_TO_SYNC_FRAMES: String = "prepend-sps-pps-to-idr-frames"
        @JvmStatic val KEY_PRIORITY: String = "priority"
        @JvmStatic val KEY_PROFILE: String = "profile"
        @JvmStatic val KEY_PUSH_BLANK_BUFFERS_ON_STOP: String = "push-blank-buffers-on-shutdown"
        @JvmStatic val KEY_QUALITY: String = "quality"
        @JvmStatic val KEY_REPEAT_PREVIOUS_FRAME_AFTER: String = "repeat-previous-frame-after"
        @JvmStatic val KEY_ROTATION: String = "rotation-degrees"
        @JvmStatic val KEY_SAMPLE_RATE: String = "sample-rate"
        @JvmStatic val KEY_SLICE_HEIGHT: String = "slice-height"
        @JvmStatic val KEY_SLOW_MOTION_MARKERS: String = "slow-motion-markers"
        @JvmStatic val KEY_STRIDE: String = "stride"
        @JvmStatic val KEY_TEMPORAL_LAYERING: String = "ts-schema"
        @JvmStatic val KEY_TILE_HEIGHT: String = "tile-height"
        @JvmStatic val KEY_TILE_WIDTH: String = "tile-width"
        @JvmStatic val KEY_TRACK_ID: String = "track-id"
        @JvmStatic val KEY_VIDEO_ENCODING_STATISTICS_LEVEL: String = "video-encoding-statistics-level"
        @JvmStatic val KEY_VIDEO_QP_AVERAGE: String = "video-qp-average"
        @JvmStatic val KEY_VIDEO_QP_B_MAX: String = "video-qp-b-max"
        @JvmStatic val KEY_VIDEO_QP_B_MIN: String = "video-qp-b-min"
        @JvmStatic val KEY_VIDEO_QP_I_MAX: String = "video-qp-i-max"
        @JvmStatic val KEY_VIDEO_QP_I_MIN: String = "video-qp-i-min"
        @JvmStatic val KEY_VIDEO_QP_MAX: String = "video-qp-max"
        @JvmStatic val KEY_VIDEO_QP_MIN: String = "video-qp-min"
        @JvmStatic val KEY_VIDEO_QP_P_MAX: String = "video-qp-p-max"
        @JvmStatic val KEY_VIDEO_QP_P_MIN: String = "video-qp-p-min"
        @JvmStatic val KEY_WIDTH: String = "width"
        @JvmStatic val MIMETYPE_AUDIO_AAC: String = "audio/mp4a-latm"
        @JvmStatic val MIMETYPE_AUDIO_AAC_ELD: String = "audio/mp4a.40.39"
        @JvmStatic val MIMETYPE_AUDIO_AAC_HE_V1: String = "audio/mp4a.40.05"
        @JvmStatic val MIMETYPE_AUDIO_AAC_HE_V2: String = "audio/mp4a.40.29"
        @JvmStatic val MIMETYPE_AUDIO_AAC_LC: String = "audio/mp4a.40.02"
        @JvmStatic val MIMETYPE_AUDIO_AAC_XHE: String = "audio/mp4a.40.42"
        @JvmStatic val MIMETYPE_AUDIO_AC3: String = "audio/ac3"
        @JvmStatic val MIMETYPE_AUDIO_AC4: String = "audio/ac4"
        @JvmStatic val MIMETYPE_AUDIO_AMR_NB: String = "audio/3gpp"
        @JvmStatic val MIMETYPE_AUDIO_AMR_WB: String = "audio/amr-wb"
        @JvmStatic val MIMETYPE_AUDIO_DOLBY_MAT: String = "audio/vnd.dolby.mat"
        @JvmStatic val MIMETYPE_AUDIO_DOLBY_TRUEHD: String = "audio/vnd.dolby.mlp"
        @JvmStatic val MIMETYPE_AUDIO_DRA: String = "audio/vnd.dra"
        @JvmStatic val MIMETYPE_AUDIO_DTS: String = "audio/vnd.dts"
        @JvmStatic val MIMETYPE_AUDIO_DTS_HD: String = "audio/vnd.dts.hd"
        @JvmStatic val MIMETYPE_AUDIO_DTS_UHD: String = "audio/vnd.dts.uhd"
        @JvmStatic val MIMETYPE_AUDIO_EAC3: String = "audio/eac3"
        @JvmStatic val MIMETYPE_AUDIO_EAC3_JOC: String = "audio/eac3-joc"
        @JvmStatic val MIMETYPE_AUDIO_FLAC: String = "audio/flac"
        @JvmStatic val MIMETYPE_AUDIO_G711_ALAW: String = "audio/g711-alaw"
        @JvmStatic val MIMETYPE_AUDIO_G711_MLAW: String = "audio/g711-mlaw"
        @JvmStatic val MIMETYPE_AUDIO_IEC61937: String = "audio/x-iec61937"
        @JvmStatic val MIMETYPE_AUDIO_MPEG: String = "audio/mpeg"
        @JvmStatic val MIMETYPE_AUDIO_MPEGH_BL_L3: String = "audio/mhm1.03"
        @JvmStatic val MIMETYPE_AUDIO_MPEGH_BL_L4: String = "audio/mhm1.04"
        @JvmStatic val MIMETYPE_AUDIO_MPEGH_LC_L3: String = "audio/mhm1.0d"
        @JvmStatic val MIMETYPE_AUDIO_MPEGH_LC_L4: String = "audio/mhm1.0e"
        @JvmStatic val MIMETYPE_AUDIO_MPEGH_MHA1: String = "audio/mha1"
        @JvmStatic val MIMETYPE_AUDIO_MPEGH_MHM1: String = "audio/mhm1"
        @JvmStatic val MIMETYPE_AUDIO_MSGSM: String = "audio/gsm"
        @JvmStatic val MIMETYPE_AUDIO_OPUS: String = "audio/opus"
        @JvmStatic val MIMETYPE_AUDIO_QCELP: String = "audio/qcelp"
        @JvmStatic val MIMETYPE_AUDIO_RAW: String = "audio/raw"
        @JvmStatic val MIMETYPE_AUDIO_SCRAMBLED: String = "audio/scrambled"
        @JvmStatic val MIMETYPE_AUDIO_VORBIS: String = "audio/vorbis"
        @JvmStatic val MIMETYPE_IMAGE_ANDROID_HEIC: String = "image/vnd.android.heic"
        @JvmStatic val MIMETYPE_TEXT_CEA_608: String = "text/cea-608"
        @JvmStatic val MIMETYPE_TEXT_CEA_708: String = "text/cea-708"
        @JvmStatic val MIMETYPE_TEXT_SUBRIP: String = "application/x-subrip"
        @JvmStatic val MIMETYPE_TEXT_VTT: String = "text/vtt"
        @JvmStatic val MIMETYPE_VIDEO_AV1: String = "video/av01"
        @JvmStatic val MIMETYPE_VIDEO_AVC: String = "video/avc"
        @JvmStatic val MIMETYPE_VIDEO_DOLBY_VISION: String = "video/dolby-vision"
        @JvmStatic val MIMETYPE_VIDEO_H263: String = "video/3gpp"
        @JvmStatic val MIMETYPE_VIDEO_HEVC: String = "video/hevc"
        @JvmStatic val MIMETYPE_VIDEO_MPEG2: String = "video/mpeg2"
        @JvmStatic val MIMETYPE_VIDEO_MPEG4: String = "video/mp4v-es"
        @JvmStatic val MIMETYPE_VIDEO_RAW: String = "video/raw"
        @JvmStatic val MIMETYPE_VIDEO_SCRAMBLED: String = "video/scrambled"
        @JvmStatic val MIMETYPE_VIDEO_VP8: String = "video/x-vnd.on2.vp8"
        @JvmStatic val MIMETYPE_VIDEO_VP9: String = "video/x-vnd.on2.vp9"
        @JvmStatic val PICTURE_TYPE_B: Int = 3
        @JvmStatic val PICTURE_TYPE_I: Int = 1
        @JvmStatic val PICTURE_TYPE_P: Int = 2
        @JvmStatic val PICTURE_TYPE_UNKNOWN: Int = 0
        @JvmStatic val TYPE_BYTE_BUFFER: Int = 5
        @JvmStatic val TYPE_FLOAT: Int = 3
        @JvmStatic val TYPE_INTEGER: Int = 1
        @JvmStatic val TYPE_LONG: Int = 2
        @JvmStatic val TYPE_NULL: Int = 0
        @JvmStatic val TYPE_STRING: Int = 4
        @JvmStatic val VIDEO_ENCODING_STATISTICS_LEVEL_1: Int = 1
        @JvmStatic val VIDEO_ENCODING_STATISTICS_LEVEL_NONE: Int = 0
        @JvmStatic fun createAudioFormat(mime: String, sampleRate: Int, channelCount: Int): MediaFormat {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createSubtitleFormat(mime: String, language: String): MediaFormat {
            throw RuntimeException("Stub!")
        }
        @JvmStatic fun createVideoFormat(mime: String, width: Int, height: Int): MediaFormat {
            throw RuntimeException("Stub!")
        }
    }

    fun MediaFormat() {
        throw RuntimeException("Stub!")
    }

    fun MediaFormat(other: MediaFormat) {
        throw RuntimeException("Stub!")
    }

    fun containsKey(name: String): Boolean {
        throw RuntimeException("Stub!")
    }

    fun containsFeature(name: String): Boolean {
        throw RuntimeException("Stub!")
    }

    fun getValueTypeForKey(name: String): Int {
        throw RuntimeException("Stub!")
    }

    fun getNumber(name: String): Number {
        throw RuntimeException("Stub!")
    }

    fun getNumber(name: String, defaultValue: Number): Number {
        throw RuntimeException("Stub!")
    }

    fun getInteger(name: String): Int {
        throw RuntimeException("Stub!")
    }

    fun getInteger(name: String, defaultValue: Int): Int {
        throw RuntimeException("Stub!")
    }

    fun getLong(name: String): Long {
        throw RuntimeException("Stub!")
    }

    fun getLong(name: String, defaultValue: Long): Long {
        throw RuntimeException("Stub!")
    }

    fun getFloat(name: String): Float {
        throw RuntimeException("Stub!")
    }

    fun getFloat(name: String, defaultValue: Float): Float {
        throw RuntimeException("Stub!")
    }

    fun getString(name: String): String {
        throw RuntimeException("Stub!")
    }

    fun getString(name: String, defaultValue: String): String {
        throw RuntimeException("Stub!")
    }

    fun getByteBuffer(name: String): ByteBuffer {
        throw RuntimeException("Stub!")
    }

    fun getByteBuffer(name: String, defaultValue: ByteBuffer): ByteBuffer {
        throw RuntimeException("Stub!")
    }

    fun getFeatureEnabled(feature: String): Boolean {
        throw RuntimeException("Stub!")
    }

    fun setInteger(name: String, value: Int) {
        throw RuntimeException("Stub!")
    }

    fun setLong(name: String, value: Long) {
        throw RuntimeException("Stub!")
    }

    fun setFloat(name: String, value: Float) {
        throw RuntimeException("Stub!")
    }

    fun setString(name: String, value: String) {
        throw RuntimeException("Stub!")
    }

    fun setByteBuffer(name: String, bytes: ByteBuffer) {
        throw RuntimeException("Stub!")
    }

    fun removeKey(name: String) {
        throw RuntimeException("Stub!")
    }

    fun removeFeature(name: String) {
        throw RuntimeException("Stub!")
    }

    fun getKeys(): MutableSet {
        throw RuntimeException("Stub!")
    }

    fun getFeatures(): MutableSet {
        throw RuntimeException("Stub!")
    }

    fun setFeatureEnabled(feature: String, enabled: Boolean) {
        throw RuntimeException("Stub!")
    }

    fun toString(): String {
        throw RuntimeException("Stub!")
    }
}