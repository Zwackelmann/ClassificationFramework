package parser

import java.util.ArrayList
import com.cybozu.labs.langdetect.Detector
import com.cybozu.labs.langdetect.DetectorFactory
import com.cybozu.labs.langdetect.Language
import java.io.File

object LangDetector {
    val profileDirectory = "data/util/language_profiles"
    DetectorFactory.loadProfile(profileDirectory)
    
    def detect(text: String) = {
        val detector = DetectorFactory.create()
        detector.append(text)
        detector.detect()
    }
    
    def detectLangs(text: String) = {
        val detector = DetectorFactory.create()
        detector.append(text)
        detector.getProbabilities()
    }
}
