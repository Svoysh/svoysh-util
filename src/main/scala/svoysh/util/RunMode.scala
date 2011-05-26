package svoysh.util

import java.util.concurrent.atomic.AtomicBoolean
import net.liftweb.util._

object RunMode extends RunMode

/**
 * To configure application for production run mode set system property
 * "run.mode" to "production". If no run mode specified then development mode
 * assumed. Example:
 * [java -Drun.mode=production -jar sbt.jar ~jetty-run]
 * See [http://www.assembla.com/wiki/show/liftweb/Run_Modes]
 */
trait RunMode {

	def isStartedInDevMode: Boolean = ModeHolder.isStartedInDevMode

	def isProdMode: Boolean = ModeHolder.isProdModeCurrently

	def isDevMode: Boolean = !isProdMode

	/**
	 * Can toggle mode only if app was started in development mode.
	 */
	def canSwitchMode: Boolean = {
		// Note: Additional checks can be added here. Eg test if current user is admin.
		isStartedInDevMode
	}

	def switchToDevMode {
		if (canSwitchMode && !isDevMode) ModeHolder.setProdModeCurrently(false)
	}

	def switchToProdMode {
		if (canSwitchMode && !isProdMode) ModeHolder.setProdModeCurrently(true)
	}

	def getModeName: String = {
		if (isProdMode) "PROD"
		else "DEV"
	}
}

private object ModeHolder {

	lazy val isStartedInProdMode: Boolean = Props.productionMode

	lazy val isStartedInDevMode: Boolean = !isStartedInProdMode

	private var _isProdModeCurrently: AtomicBoolean = {
		new AtomicBoolean(isStartedInProdMode)
	}

	def isProdModeCurrently: Boolean = _isProdModeCurrently.get

	def setProdModeCurrently(is: Boolean) {
		_isProdModeCurrently.set(is)

		// TODO: DB can be reconnected here and other startup data updated
		// depending on changed mode.
	}
}