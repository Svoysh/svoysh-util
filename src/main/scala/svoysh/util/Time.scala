package svoysh.util

import net.liftweb.common._

object Time extends Time

trait Time {

	def measureExecTimeRange(proc: => Any): (Long, Long) = {
		val beginTime = System.currentTimeMillis
		proc
		val endTime = System.currentTimeMillis
		(beginTime, endTime)
	}

	def measureExecTime(proc: => Any): Long = {
		val (beginTime: Long, endTime: Long) = measureExecTimeRange(proc)
		endTime - beginTime
	}

	def printExecTime[R](proc: => R): R = {
		printExecTime()(proc)
	}

	def printExecTime[R](procName: String = null)(proc: => R): R = {
		val fixProcName = procName match {
			case null => ""
			case s => " of '" + s + "'"
		}
		var res: Box[R] = Empty
		val execTime = measureExecTime {res = Full(proc)}
		println("Exec time" + fixProcName + ": " + execTime + " ms.")
		res.open_!
	}
}