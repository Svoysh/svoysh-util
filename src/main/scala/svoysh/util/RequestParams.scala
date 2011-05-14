package svoysh.util

import net.liftweb.http.S._

object RequestParams extends RequestParams

trait RequestParams {

	val param_id = "id"

	def getIdParam(idParamName: String = param_id): Long = {
		getParamAsLong(idParamName, 0L)
	}

	def hasValidIdParam(idParamName: String = param_id): Boolean = {
		hasParam(idParamName) && getIdParam(idParamName) > 0L
	}

	def hasParam(paramName: String): Boolean = {
		param(paramName).isDefined
	}

	def getParamAsInt(paramName: String, defaultValue: Int = 0): Int = {
		getParamParsed(paramName, defaultValue, java.lang.Integer.parseInt(_))
	}

	def getParamAsLong(paramName: String, defaultValue: Long = 0L): Long = {
		getParamParsed(paramName, defaultValue, java.lang.Long.parseLong(_))
	}

	def getParamParsed[T](paramName: String, defaultValue: T, parse: String => T): T = {
		param(paramName).openOr(defaultValue.toString) match {
			case null => defaultValue
			case s: String => {
				try {parse(s)}
				catch {case e => defaultValue}
			}
		}
	}
}