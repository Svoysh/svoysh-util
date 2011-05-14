package svoysh

package object util
	extends Strings
	with Files
	with RequestParams
	with RunMode
	with Time
{
	// Shortcuts for mutable maps.
	type MMap[A, B] = collection.mutable.Map[A, B]
	val MMap = collection.mutable.Map
}