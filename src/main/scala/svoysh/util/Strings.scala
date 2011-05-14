package svoysh.util

object Strings extends Strings

trait Strings {

	/**
	 * Returns <code>true</code> if <code>string</code> either <code>null</code>
	 * or equals to <code>""</code> (empty string) or consists of white spaces
	 * only.
	 */
	def isEmpty(string: String): Boolean = (
		string == null || string.trim.length == 0
	)

	/**
	 * Trim trailing whitespaces.
	 */
	def rtrim(str: String): String = {
		if (str == null) return null
		var len = str.length
		while (len > 0) {
			len -= 1
			if (!Character.isWhitespace(str.charAt(len))) {
				return str.substring(0, len + 1)
			}
		}
		""
	}
}