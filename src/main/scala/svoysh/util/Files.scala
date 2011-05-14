package svoysh.util

import net.liftweb.common._

object Files extends Files

trait Files {

	/**
	 * Get file name without base path.
	 * Example:
	 * filename [path/to\some.jpg]
	 * result [some.jpg]
	 */
	def getFileName(filepath: String): Box[String] = {
		filepath match {
			case null =>
			case _ => {
				val filepathM = rtrim(filepath)
				val length = filepathM.length
				if (length > 0) {
					val pathSeps = List('/', '\\')
					val lastSepIndex = filepathM.lastIndexWhere(pathSeps.contains(_))
					if (lastSepIndex < 0) {
						return Full(filepathM)
					} else if ((lastSepIndex) + 1 < length) {
						return Full(filepathM.substring(lastSepIndex + 1, length))
					}
				}
			}
		}
		Empty
	}

	/**
	 * Get file name without extension.
	 * Example:
	 * filename [some.jpg]
	 * result [some]
	 */
	def getFileNameWithoutExt(filepath: String): Box[String] = {
		getFileName(filepath) match {
			case Full(filename) => {
				if (filename.length > 0) {
					val dotIndex = filename.lastIndexOf(".")
					if (dotIndex > 0) return Full(filename.substring(0, dotIndex))
					else if (dotIndex < 0) return Full(filename)
				}
			}
			case _ =>
		}
		Empty
	}

	/**
	 * Get file extension without dot.
	 * Example:
	 * filename [some.jpg]
	 * result [jpg]
	 */
	def getFileExt(filepath: String): Box[String] = {
		getFileName(filepath) match {
			case Full(filename) => {
				val length = filename.length
				if (length > 0) {
					val dotIndex = filename.lastIndexOf(".")
					if (dotIndex >= 0 && (dotIndex + 1) < length) {
						return Full(filename.substring(dotIndex + 1, length))
					}
				}
			}
			case _ =>
		}
		Empty
	}

	/**
	 * Get file extension with dot.
	 * Example:
	 * filename [some.jpg]
	 * result [.jpg]
	 */
	def getFileExtWithDot(filepath: String): Box[String] = {
		getFileExt(filepath) match {
			case Full(ext) => return Full("." + ext)
			case _ =>
		}
		Empty
	}
}