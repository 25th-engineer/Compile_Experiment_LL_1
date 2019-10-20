import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.util.matching.Regex
object LL1_try_GUI {
	private final var allCharacters = new String()
	private final var relations = new ArrayBuffer[ (String, String, String) ]()
	private final var VN = new String()
	private final var VT = new String()
	private final var LL1_G = new ArrayBuffer[ (String, String) ]()
	private val allCandidateLetters = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩABCDEFGHIJKLMNOPQRSTUVWXYZ"
	private final var usedCharacters = ""
	def main(args: Array[String]): Unit = {
		val result = parseFile("/home/hadoop001/Desktop/test.data")
		println( "the original language rules:" )
		for( rs <- result ) {
			println( rs._1 + "->" + rs._2 )
		}
		initiate("/home/hadoop001/Desktop/test.data")
		println( "after eliminating the all the left recursion in the language rules:" )
		displayRelations()
		println( "VT = " + VT )
		println( "VN = " + VN )
		println( "allCharacters = " + allCharacters )
		println("*************")
		val testMatrix1 = initiateMatrix()
		for( i <- 0 to (testMatrix1.length - 1) ) {
			for( j <- 0 to (testMatrix1(0).length - 1) ) {
				print(testMatrix1(i)(j) + "   ")
			}
			println()
		}
		println("*************")
		val tx = FIRST(LL1_G)
		println( "FIRST: " )
		for( t <- tx ) {
			if( allCharacters.contains( t._1 ) ) {
				println(t)
			}
		}
		val ex = FOLLOW(LL1_G)
		println( "FOLLOW: " )
		for( t <- ex ) {
			if( VN.contains( t._1 ) ) {
				println(t)
			}
		}
		println("*************")
		val testMatrix2 = createMatrix()
		for( i <- 0 to (testMatrix2.length - 1) ) {
			for( j <- 0 to (testMatrix2(0).length - 1) ) {
				print(testMatrix2(i)(j) + "   ")
			}
			println()
		}
		println("*************")
		for( i <- 0 to (testMatrix1.length - 1) ) {
			for( j <- 0 to (testMatrix1(0).length - 1) ) {
				if( i == 0 && j == 0 ) {
					testMatrix1(i)(j) = "   "
				}
			}
		}
		println()
		for( i <- 0 to (testMatrix1.length - 1) ) {
			for( j <- 0 to (testMatrix1(0).length - 1) ) {
				if( testMatrix1(i)(j) == null && i != 0 && j != 0 ) {
					print("   ")
				}
				else {
					print(testMatrix1(i)(j) + "      ")
				}
			}
			println()
		}
		analyse("i+i*i#")
	}
	def initiate( filePath: String ): Unit = {
		LL1_G = parseFile(filePath)
		allCharacters = getWholeCharacters(LL1_G)
		usedCharacters = allCharacters
		relations = getRelation(LL1_G)
		VN = getVN(allCharacters)
		VT = getVT(allCharacters)
		eliminateLeftRecursion
	}
	def displayRelations(): Unit = {
		for( ex <- relations ) {
			if( ex._3 != "א" ) {
				println( ex._1 + "->" + ex._2 + "|" + ex._3 )
			}
			else {
				println( ex._1 + "->" + ex._2 )
			}
		}
	}
	def parseFile( filePath: String ): ArrayBuffer[ ( String, String ) ] = {
		val result = new ArrayBuffer[ ( String, String ) ]( countLines( readFromTxtByLine(filePath) ) )
		val sourceFile = readFromTxtByLine(filePath)
		for( line <- sourceFile ) {
			val tmp = line.split( "->", 2 )
			result += ( ( tmp.head, tmp.last ) )
		}
		result
	}
	def countLines( sourceFile: Array[String] ): Int = {
		var cnt = 0
		for( line <- sourceFile ) {
			cnt += 1
		}
		cnt
	}
	def readFromTxtByLine(filePath: String): Array[String] = {
		import scala.io.Source
		val source = Source.fromFile(filePath, "UTF-8")
		val lines = source.getLines().toArray
		source.close()
		lines
	}
	def getWholeCharacters( string: ArrayBuffer[ (String, String) ] ): String = {
		var wholeCharacters = ""
		for( expression <- string ) {
			wholeCharacters += expression._1 + expression._2
		}
		val pattern = new Regex("\\|")
		val result = pattern replaceAllIn( wholeCharacters, "" )
		if( result.isEmpty )
			"function getWholeCharacters failed"
		else
			result.distinct
	}
	def getVN( string: String ): String = {
		val pattern = new Regex("[A-Z]")
		if( (pattern findAllIn string) != null )
			(pattern findAllIn string).mkString("")
		else
			"function getVN failed"
	}
	def getVT( string: String ): String = {
		val pattern1 = new Regex("[A-Z]")
		val pattern2 = new Regex("\\|")
		val firstFilter = pattern1 replaceAllIn( string, "" )
		val result = pattern2 replaceAllIn( firstFilter, "" )
		if( result.isEmpty == false )
			result
		else
			return "function getVT failed"
	}
	def getRelation( string: ArrayBuffer[ (String, String) ] ): ArrayBuffer[ (String, String, String) ] = {
		val relation = new ArrayBuffer[ (String, String, String) ]()
		for( expression <- string ) {
			if( expression._2.contains("|") == false ) {
				relation += ( ( expression._1, expression._2, "א" ) )
			}
			else {
				val tmp = expression._2.split("\\|", 2 )
				relation += ( ( expression._1, tmp.head, tmp.last ) )
			}
		}
		relation
	}
	def findFirst( ch: String ): String = {
		val localRelations = relations
		var result = ""
		for( ex <- localRelations ) {
			if( ch == ex._1 ) {
				if( ex._3 != "א" ) {
					if( VT.contains( ex._2(0) ) && ex._2(0) != 'ε' ) {
						result += ex._2(0).toString
					}
					if( VT.contains( ex._3(0) ) && ex._3(0) != 'ε' ) {
						result += ex._3(0).toString
					}
				}
				else {
					if( VT.contains( ex._2(0) ) && ex._2(0) != 'ε' ) {
						result += ex._2(0).toString
					}
				}
			}
		}
		result
	}
	def judgeOnlyOneVoidSuccession( ch: String ): Boolean = {
		val localRelations = relations
		var result = 1
		for( ex <- localRelations ) {
			if( ch == ex._1 ) {
				if( ex._3 != "א" ) {
					if( ( ex._2.length == 1 && ex._2(0) == 'ε' ) || (ex._3.length == 1 && ex._3(0) == 'ε') ) {
						result = 1
					}
					else {
						result = 0
					}
				}
				else {
					if( ( ex._2.length == 1 && ex._2(0) == 'ε' ) ) {
						result = 1
					}
					else {
						result = 0
					}
				}
			}
		}
		if( result == 1 ) true else false
	}
	def judgeCaseXY( ch: Char ): Boolean = {
		val localVN = VN
		val localRelations = relations
		var result = 0
		if( localVN.contains(ch) == true ) {
			for( ex <- localRelations ) {
				if( ex._1(0) == ch ) {
					if( localVN.contains( ex._2(0) ) || localVN.contains( ex._3(0) ) ) {
						result += 1
					}
				}
			}
		}
		if( result > 0 )
			true
		else
			false
	}
	def findCase_Y_In_XY( ch: Char ): String = {
		val localVN = VN
		val localRelations = relations
		var result = ""
		if( localVN.contains(ch) == true ) {
			for( ex <- localRelations ) {
				if( ex._1(0) == ch ) {
					if( ex._3 != "א" ) {
						if( localVN.contains( ex._2(0) ) == true ) {
							result += ex._2(0).toString
						}
						if( localVN.contains( ex._3(0) ) == true ) {
							result += ex._3(0).toString
						}
					}
					else {
						if( localVN.contains( ex._2(0) ) == true ) {
							result += ex._2(0).toString
						}
					}
				}
			}
		}
		result
	}
	def findCase_Y_In_nY( ch: Char ): String = {
		val localVN = VN
		val localRelations = relations
		var result = ""
		for( ex <- localRelations ) {
			if (ex._1 == ch.toString) {
				var tmp = ""
				if (ex._3 != 'א') {
					var cnt = 0
					for (tx <- ex._2) {
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
						else {
							tmp = ""
						}
					}
					if (cnt == ex._2.length) {
						result += tmp
					}
					cnt = 0
					tmp = ""
					for (tx <- ex._3) {
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
						else {
							tmp = ""
						}
					}
					if (cnt == ex._3.length) {
						result += tmp
					}
				}
				else {
					tmp = ""
					var cnt = 0
					for (tx <- ex._2) {
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
						else {
							tmp = ""
						}
					}
					if (cnt == ex._2.length) {
						result += tmp
					}
				}
			}
		}
		result = result.distinct
		result
	}
	def FIRST( string: ArrayBuffer[ (String, String) ] ): Map[ String, String ] = {
		val FIRST_Group = Map[ String, String ]()
		val wholeCharacters = allCharacters
		val localVT = VT
		val localVN = VN
		for( character <- wholeCharacters ) {
			if( localVT.contains(character) ) {
				if( FIRST_Group.contains(character.toString) == true ) {
					val tmp = character.toString + FIRST_Group(character.toString)
					FIRST_Group(character.toString) = tmp.distinct
				}
				else {
					FIRST_Group(character.toString) = character.toString
				}
			}
			if( localVN.contains(character.toString) == true ) {
				val value = findFirst(character.toString)
				if ( value.length != 0 ) {
					if ( FIRST_Group.contains(character.toString) == true ) {
						for( ch <- value ) {
							val tmp = ch + FIRST_Group(character.toString)
							FIRST_Group(character.toString) = tmp.distinct
						}
					}
					else {
						FIRST_Group(character.toString) = value.toString
					}
				}
				if( judgeOnlyOneVoidSuccession(character.toString) == true ) {
					if ( FIRST_Group.contains(character.toString) == true ) {
						val tmp = "ε" + FIRST_Group(character.toString)
						FIRST_Group(character.toString) = tmp.distinct
					}
					else {
						FIRST_Group(character.toString) = "ε"
					}
				}
			}
			for( character <- wholeCharacters ) {
				if( judgeCaseXY(character) == true ) {
					val tmpReply = findCase_Y_In_XY(character)
					for( eachTmpReply <- tmpReply ) {
						if( FIRST_Group.contains(eachTmpReply.toString) == true ) {
							for (ex <- FIRST_Group(eachTmpReply.toString)) {
								if (ex != 'ε') {
									if (FIRST_Group.contains(character.toString) == true) {
										val tmp = ex.toString + FIRST_Group(character.toString)
										FIRST_Group(character.toString) = tmp.distinct
									}
									else {
										FIRST_Group(character.toString) = ex.toString
									}
								}
							}
						}
					}
				}
				if( findCase_Y_In_nY(character).length > 0 ) {
					var flag = true
					val tmpReply = findCase_Y_In_nY(character)
					for( ex <- tmpReply ) {
						if( localVN.contains(ex.toString) && FIRST_Group.contains(ex.toString) == true )  {
							if( FIRST_Group(ex.toString).contains("ε") == false ) {
								flag = false
							}
						}
						else {
							flag = false
						}
						if( flag == true ) {
							if (FIRST_Group.contains(character.toString) == true) {
								val tmp = FIRST_Group(ex.toString).replace( "ε", "" ) + FIRST_Group(character.toString)
								FIRST_Group(character.toString) = tmp.distinct
							}
							else {
								FIRST_Group(character.toString) = FIRST_Group(ex.toString).replace( "ε", "" )
							}
						}
					}
				}
				if( findCase_Y_In_nY(character).length > 0 ) {
					var flag = true
					val tmpReply = findCase_Y_In_nY(character)
					for( ex <- tmpReply ) {
						if( localVN.contains(ex.toString) && FIRST_Group.contains(ex.toString) == true )  {
							if( FIRST_Group(ex.toString).contains("ε") == false ) {
								flag = false
							}
						}
						else {
							flag = false
						}
						if( flag == true ) {
							if (FIRST_Group.contains(character.toString) == true) {
								val tmp = "ε" + FIRST_Group(character.toString)
								FIRST_Group(character.toString) = tmp.distinct
							}
							else {
								FIRST_Group(character.toString) = "ε"
							}
						}
					}
				}
			}
		}
		FIRST_Group
	}
	def FOLLOW( string: ArrayBuffer[ (String, String) ] ): Map[ String, String ] = {
		val localVN = VN
		val FOLLOW_Group = Map[ String, String ]()
		for( ch <- localVN ) {
			FOLLOW_Group(ch.toString) = dfsFOLLOW(ch.toString)
		}
		FOLLOW_Group
	}
	def dfsFOLLOW( ch: String ): String = {
		val FOLLOWPositions = Map[ String, String ]()
		val FOLLOW_Group = Map[ String, String ]()
		val localLL1_G = LL1_G
		val FIRST_Group = FIRST(localLL1_G)
		val localVN = VN
		for( ch <- localVN ) {
			FOLLOWPositions(ch.toString) = findGivenValueFOLLOWPosition(ch.toString)
			FOLLOW_Group(ch.toString) = "#"
		}
		var result = ""
		if( FOLLOWPositions(ch).length == 4 ) {
			if( FOLLOWPositions(ch)(1).toString == "T" ) {
				result += FIRST_Group( FOLLOWPositions(ch)(0).toString )
				FOLLOW_Group(ch) += result.distinct
			}
			else if( FOLLOWPositions(ch)(3).toString == "T" ) {
				result += FIRST_Group( FOLLOWPositions(ch)(2).toString )
				FOLLOW_Group(ch) += result.distinct
			}
			if( FOLLOWPositions(ch)(1).toString == "W" ) {
				result += dfsFOLLOW( FOLLOWPositions(ch)(0).toString )
				FOLLOW_Group(ch) = result.distinct
			}
			else if( FOLLOWPositions(ch)(3).toString == "W" ) {
				result += dfsFOLLOW( FOLLOWPositions(ch)(2).toString )
				FOLLOW_Group(ch) = result.distinct
			}
		}
		if( FOLLOWPositions(ch).length == 2 ) {
			if( FOLLOWPositions(ch)(1).toString == "T" ) {
				result += FIRST_Group( FOLLOWPositions(ch)(0).toString )
				FOLLOW_Group(ch) = result.distinct
			}
			else if( FOLLOWPositions(ch)(1).toString == "W" ) {
				result += dfsFOLLOW( FOLLOWPositions(ch)(0).toString )
				FOLLOW_Group(ch) = result.distinct
			}
		}
		FOLLOW_Group(ch).replace("ε", "")
	}
	def findGivenValueFOLLOWPosition( ch: String ): String = {
		var result = ""
		val cnt = new ArrayBuffer[String]()
		val localRelations = relations
		for( ex <- localRelations ) {
			if( ex._3 != "א" ) {
				if( ex._2.contains(ch) ) {
					if( ex._2.length == 3 ) {
						if( ex._2(1).toString == ch && judgeCase2( ex._1, ex._2(0).toString, ch, ex._2(2).toString ) ) {
							val value = ex._2(2).toString + "T"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
						if( ex._2(1).toString == ch && judgeCase3( ex._1, ex._2(0).toString, ch, ex._2(2).toString ) ) {
							val value = ex._1.toString + "W"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
					}
					if( ex._2.length == 2 ) {
						if( ex._2(1).toString == ch && judgeCase3( ex._1, ex._2(0).toString, ch, "" ) ) {
							val value = ex._1 + "W"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
					}
				}
				if( ex._3.contains(ch) ) {
					if( ex._3.length == 3 ) {
						if( ex._3(1).toString == ch && judgeCase2( ex._1, ex._3(0).toString, ch, ex._3(2).toString ) ) {
							val value = ex._3(2).toString + "T"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
						if( ex._3(1).toString == ch && judgeCase3( ex._1, ex._3(0).toString, ch, ex._3(2).toString ) ) {
							val value = ex._1 + "W"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
					}
					if( ex._3.length == 2 ) {
						if( ex._3(1).toString == ch && judgeCase3( ex._1, ex._3(0).toString, ch, "" ) ) {
							val value = ex._1 + "W"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
					}
				}
			}
			else {
				if( ex._2.contains(ch) ) {
					if( ex._2.length == 3 ) {
						if( ex._2(1).toString == ch && judgeCase2( ex._1, ex._2(0).toString, ch, ex._2(2).toString ) ) {
							val value = ex._2(2).toString + "T"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
						if( ex._2(1).toString == ch && judgeCase3( ex._1, ex._2(0).toString, ch, ex._2(2).toString ) ) {
							val value = ex._1 + "T"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
					}
					if( ex._2.length == 2 ) {
						if( ex._2(1).toString == ch && judgeCase3( ex._1, ex._2(0).toString, ch, "" ) ) {
							val value = ex._1 + "W"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
					}
				}
			}
		}
		result
	}
	def judgeCase2( A: String, α: String, B: String, β: String ): Boolean = {
		val localVN = VN
		val wholeCharacters = allCharacters
		val localLL1_G = LL1_G
		val localFIRST = FIRST(localLL1_G)
		if( localVN.contains(A) == true && wholeCharacters.contains(α) == true && localVN.contains(B) == true &&
				wholeCharacters.contains(β) && localFIRST.contains(β) == true ) {
			true
		}
		else {
			false
		}
	}
	def judgeCase3( A: String, α: String, B: String, β: String ): Boolean = {
		val localVN = VN
		val wholeCharacters = allCharacters
		val localLL1_G = LL1_G
		val localFIRST = FIRST(localLL1_G)
		if( ( localVN.contains(A) == true && wholeCharacters.contains(α) == true && localVN.contains(B) == true ) ||
				( localVN.contains(A) == true && wholeCharacters.contains(α) == true && localVN.contains(B) == true && localFIRST(β).contains("ε") == true ) ) {
			true
		}
		else {
			false
		}
	}
	def initiateMatrix(): Array[ Array[ String] ] = {
		val localVN = VN
		val localVT = VT
		val result = Array.ofDim[String](localVN.length + 1, localVT.length + 1)
		for( i <- 1 to localVN.length ) {
			result(i)(0) = localVN(i - 1).toString
		}
		for( j <- 1 to localVT.length ) {
			if( localVT(j - 1).toString == "ε" ) {
				result(0)(j) = "#"
			}
			else {
				result(0)(j) = localVT(j - 1).toString
			}
		}
		result
	}
	def createMatrix(): Array[ Array[String] ] = {
		val result = initiateMatrix()
		val localVT = VT
		val localRelations = relations
		val localLL1_G = LL1_G
		val localFIRST = FIRST(localLL1_G)
		val localFOLLOW = FOLLOW(localLL1_G)
		for( ex <- localRelations ) {
			if( ex._3 !=  "א" ) {
				for( a <- localVT ) {
					val ex2Length = ex._2.length
					var range = ""
					var α = ""
					var flag = false
					for( x <- 0 to (ex2Length - 1) ) {
						if( localFIRST( ex._2(x).toString ).contains("ε") == false && flag == false ) {
							α = ex._2(x).toString
							range = localFIRST( α )
							flag = true
						}
					}
					if( range.contains(a) == true && flag == true ) {
						result(getRow(ex._1))(getColumn(a.toString)) = ex._1 + "->" + ex._2
					}
					if( flag == false ) {
						range = "ε"
						result( getRow(ex._1) )( getColumn("ε") ) = ex._1 + "->" + "ε"
					}
					if( range.contains("ε") == true && flag == false ) {
						for( b <- localFOLLOW(α) ) {
							result( getRow(ex._1.toString) )( getColumn(b.toString) ) = ex._1 + "->" + ex._2
						}
					}
					val ex3Length = ex._3.length
					range = ""
					flag = false
					for( x <- 0 to (ex3Length - 1) ) {
						if( localFIRST( ex._3(x).toString ).contains("ε") == false && flag == false ) {
							α = ex._3(x).toString
							range = localFIRST( α )
							flag = true
						}
					}
					if( range.contains(a) == true && flag == true ) {
						result( getRow(ex._1) )( getColumn(a.toString) ) = ex._1 + "->" + ex._3
					}
					if( flag == false ) {
						range = "ε"
						result(getRow(ex._1))(getColumn("ε")) = ex._1 + "->" + "ε"
					}
					if( range.contains("ε") == true && flag == false ) {
						for( b <- localFOLLOW(ex._1) ) {
							result( getRow(ex._1.toString) )( getColumn(b.toString) ) = ex._1 + "->" + ex._3
						}
					}
				}
			}
			else {
				for( a <- localVT ) {
					val ex2Length = ex._2.length
					var range = ""
					var α = ""
					var flag = false
					for( x <- 0 to (ex2Length - 1) ) {
						if( localFIRST( ex._2(x).toString ).contains("ε") == false && flag == false ) {
							α = ex._2(x).toString
							range = localFIRST(α)
							flag = true
						}
					}
					if( range.contains(a) == true && flag == true ) {
						result( getRow(ex._1) )( getColumn(a.toString) ) = ex._1 + "->" + ex._2
					}
					if( flag == false ) {
						range = "ε"
						result( getRow(ex._1) )( getColumn("ε") ) = ex._1 + "->" + "ε"
					}
					if( range.contains("ε") == true && flag == false ) {
						for( b <- localFOLLOW(ex._1) ) {
							result( getRow(ex._1.toString) )( getColumn(b.toString) ) = ex._1 + "->" + ex._2
						}
					}
				}
			}
		}
		result
	}
	def getRow( ch: String ): Int = {
		val matrix = initiateMatrix()
		var result = -1
		if( ch == "α" ) {
			println( "1 --- getRow, ch == " + ch )
		}
		for( i <- 0 to (matrix.length - 1) ) {
			if( matrix(i)(0) == ch ) {
				result = i
			}
		}
		result
	}
	def getColumn( ch: String ): Int = {
		val matrix = initiateMatrix()
		var result = -1
		for( i <- 0 to (matrix.length - 1) ) {
			for( j <- 0 to (matrix(i).length - 1) ) {
				if( matrix(0)(j) == ch ) {
					result = j
				}
				if( matrix(0)(j) == "#" && ch == "ε" ) {
					result = j
				}
			}
		}
		result
	}
	def analyse( expression: String ): Boolean = {
		val stack = new mutable.Stack[String]()
		var localExpression = expression
		val table = createMatrix()
		val localVT = VT
		val localVN = VN
		val localRelations = relations
		stack.push("#")
		stack.push( localRelations(0)._1 )
		var cnt = 0
		println( cnt + " " + " stack = " + stack + ", expression = " + localExpression + "  initiate" )
		while( stack.isEmpty == false ) {
			val stackTop = stack.top
			stack.pop()
			if( localVN.contains(stackTop) == true ) {
				if( table( getRow(stackTop) )( getColumn( localExpression(0).toString ) ) != null ) {
					val lastHalf = table( getRow(stackTop) )( getColumn( localExpression(0).toString ) ).split( "->", 2 ).last
					val length = lastHalf.length
					for( i <- 0 to (length - 1) ) {
						if( lastHalf != "ε" ) {
							stack.push(lastHalf(length - 1 - i).toString)
						}
					}
					cnt += 1
					println( cnt + " " + " stack = " + stack + ", expression = " + localExpression +
							",  analyse expression = " + table( getRow(stackTop) )( getColumn( localExpression(0).toString ) ) + ",  POP, PUSH(" + lastHalf.reverse + ")")
				}
				else {
					if( stackTop == "#" && localExpression(0).toString == "#" ) {
						println("11111")
						return true
					}
					else {
						println("1 - error")
						println( cnt + " " + " stack = " + stack + ", expression = " + localExpression )
						return false
					}
				}
			}
			if( localVT.contains(stackTop) == true ) {
				if( stackTop == localExpression(0).toString ) {
					if( stackTop == localExpression(0).toString ) {
						localExpression = localExpression.drop(1)
						cnt += 1
						println( cnt + " " + " stack = " + stack + ", expression = " + localExpression + ",  GETNEXT(" + stackTop + ")" )
					}
					else {
						println("2 - error")
						return false
					}
				}
			}
		}
		true
	}
	def judgeLeftRecursion( expression: (String, String, String) ): Int = {
		var ans = 0
		if( expression._2.length >= 2 && expression._1 == expression._2(0).toString && expression._2.drop(1) != "ε" ) {
			ans += 1
		}
		if( expression._3.length >= 2 && expression._1 == expression._3(0).toString && expression._3.drop(1) != "ε" ) {
			ans += 2
		}
		ans
	}
	def eliminateLeftRecursion(): ArrayBuffer[ (String, String, String) ] = {
		var localRelations = relations
		var invalidRelations = new ArrayBuffer[ (String, String, String) ]()
		val localCandidateLetters = allCandidateLetters
		val VN1 = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩABCDEFGHIJKLMNOPQRSTUVWXYZ"
		val VT1 = "αβγδεζηθικλμνξοπρστυφχψωabcdefghijklmnopqrstuvwxyz"
		for( ex <- localRelations ) {
			if( ex._3 != "א" ) {
				if( judgeLeftRecursion( ex._1, ex._2, ex._3 ) == 1 ) {
					invalidRelations += ( ( ex._1, ex._2, ex._3 ) )
					val newValue = subString( usedCharacters, localCandidateLetters )(0)
					usedCharacters += newValue
					if( VN1.contains(newValue) && !VN.contains(newValue) ) {
						VN += newValue.toString
						allCharacters += newValue.toString
					}
					if( VT1.contains(newValue) && !VT.contains(newValue) ) {
						VT += newValue.toString
						allCharacters += newValue.toString
					}
					val α = ex._2.drop(1)
					val exp1 = ( ex._1, ex._3 + newValue.toString, "א" )
					val exp2 = ( newValue.toString, α.toString + newValue.toString, "ε" )
					localRelations += exp1
					localRelations += exp2
				}
				else if( judgeLeftRecursion( ex._1, ex._2, ex._3 ) == 2 ) {
					invalidRelations += ( ( ex._1, ex._2, ex._3 ) )
					val newValue = subString( usedCharacters, localCandidateLetters )(0)
					if( VN1.contains(newValue) && !VN.contains(newValue) ) {
						VN += newValue.toString
						allCharacters += newValue.toString
					}
					if( VT1.contains(newValue) && !VT.contains(newValue) ) {
						VT += newValue.toString
						allCharacters += newValue.toString
					}
					usedCharacters += newValue
					val α = ex._3.drop(1)
					val exp1 = ( ex._1, ex._3 + newValue.toString, "א" )
					val exp2 = ( newValue.toString, α.toString + newValue.toString, "ε" )
					localRelations += exp1
					localRelations += exp2
				}
				else if( judgeLeftRecursion( ex._1, ex._2, ex._3 ) == 3 ){
					println( "error in the function eliminateLeftRecursion" )
				}
			}
			else {
				if( judgeLeftRecursion( ex._1, ex._2, ex._3 ) == 1 ) {
					invalidRelations += ( ( ex._1, ex._2, ex._3 ) )
					val newValue = subString( usedCharacters, localCandidateLetters )(0)
					if( VN1.contains(newValue) && !VN.contains(newValue) ) {
						VN += newValue.toString
						allCharacters += newValue.toString
					}
					if( VT1.contains(newValue) && !VT.contains(newValue) ) {
						VT += newValue.toString
						allCharacters += newValue.toString
					}
					usedCharacters += newValue
					val α = ex._2.drop(1)
					val exp1 = ( ex._1, newValue.toString, "א" )
					val exp2 = ( newValue.toString, α.toString + newValue.toString, "ε" )
					localRelations += exp1
					localRelations += exp2
				}
			}
		}
		for( ex <- invalidRelations ) {
			localRelations = localRelations.-(ex)
		}
		relations = localRelations
		localRelations
	}
	def subString( usedCharacters: String, localCandidateLetters: String ): String = {
		require( usedCharacters.length != 0 && localCandidateLetters.length != 0 )
		var ans = ""
		var A = usedCharacters
		var B = localCandidateLetters
		if( A.length < B.length ) {
			val tmp = A
			A = B
			B = tmp
		}
		for( i <- 0 to (A.length - 1) ) {
			var j = 0
			while( j < B.length && B(j) != A(i) ) {
				j += 1
			}
			if( j == B.length ) {
				ans += A(i)
			}
		}
		ans
	}
}