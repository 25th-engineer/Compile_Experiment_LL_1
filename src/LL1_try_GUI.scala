import java.awt.{BorderLayout, Color}
import java.awt.event.{ActionEvent, ActionListener}
import java.io.FileInputStream

import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer, DefaultTableColumnModel, DefaultTableModel, TableColumn, TableModel}
import javax.swing.{JButton, JFileChooser, JFrame, JPanel, JScrollPane, JTable, JTextField, JTextPane}
import pojo.Analyse

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map}
import scala.util.matching.Regex


object LL1_try_GUI {
	private final var allCharacters = new String()
	private final var relations = new ArrayBuffer[ (String, String, String) ]()
	private final var VN = new String()
	private final var VT = new String()
	private val allCandidateLetters = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩABCDEFGHIJKLMNOPQRSTUVWXYZ"
	private final var usedCharacters = ""


	private var LL1_G = new ArrayBuffer[ (String, String) ]()//ArrayBuffer( ("E", "TG"), ("G", "+TG|-TG"), ("G", "ε"), ("T", "FS"), ("S", "*FS|/FS"),
	//("S", "ε"), ("F", "(E)"), ("F", "i") )//, ("Y", "*FS|/FS"), ("Y", "+TG|-TG"), ("Y", "x"), ("Y", "M"), ("M", "i"), ("M", "ε") )
	//	 test data 1:
	//	 ( ("E", "TG"), ("G", "+TG|-TG"), ("G", "ε"), ("T", "FS"), ("S", "*FS|/FS"),
	//			("S", "ε"), ("F", "(E)"), ("F", "i"), ("Y", "S"), ("Y", "Gx"), ("Y", "x"), ("Y", "M"), ("M", "i"), ("M", "ε") )
	//	 test data 2:
	//	        ( ("D", "*FD"), ("D", "ε"), ("T", "FD"), ("E", "TC"), ("F", "(E)"), ("F", "i"), ("C", "+TC"), ("C", "ε") )
	//	 test data 3:
	//	        ( ("E", "E+T|T"), ("T", "T*F|T"), ("F", "(E)|i") )
	//	 stand test data:
	//	        ( ("E", "TG"), ("G", "+TG|-TG"), ("G", "ε"), ("T", "FS"), ("S", "*FS|/FS"), ("S", "ε"), ("F", "(E)"), ("F", "i") )

	val staticAnalyseList : ArrayBuffer[Analyse] = new ArrayBuffer[Analyse]();
	var staticTestMatrix : Array[ Array[String] ] = new Array[Array[String]](0)
	var staticStringBuilder : StringBuilder = new StringBuilder();
	var staticStringBuilder2 : StringBuilder = new StringBuilder();

	def main(args: Array[String]): Unit = {
		//initiate("/home/hadoop001/Desktop/test.data")
		GUI1

		//GUI
	}

	/*
	* Function name: displayStack
	* Function description: 输出栈的所有元素
	* Input parameters: -mutable.Stack[String]（待处理的String类型的栈）
	* Return value: -String（栈所有元素组成的字符串）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Mon Oct 21 2019 +0800
	* Editor: 来自高山
	* Edited Date: Mon Oct 21 2019 +0800
	 */
	def displayStack( stack: mutable.Stack[String] ): String = {
		var result = ""
		for( ex <- stack ) {
			result += ex
		}
		result
	}

	/*
	* Function name: utility
	* Function description: 辅助输出函数
	* Input parameters: 无
	* Return value: 无
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sun Oct 20 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sun Oct 20 2019 +0800
	 */
	def utility(): Unit = {
		staticStringBuilder.append("VT = " + VT +"\r\n");
		staticStringBuilder.append("VN = " + VN +"\r\n");
		staticStringBuilder.append( "allCharacters = " + allCharacters +"\r\n");
		val tx = FIRST(LL1_G)
		staticStringBuilder.append(  "FIRST集: " +"\r\n");
		for( t <- tx ) {
			if( allCharacters.contains( t._1 ) ) {
				staticStringBuilder.append( "FIRST("+ t._1 + ") = {" + t._2.mkString(",") + "}\r\n");
			}
		}
		val ex = FOLLOW(LL1_G)
		staticStringBuilder.append("FOLLOW集: "+"\r\n");
		for( t <- ex ) {
			if( VN.contains( t._1 ) ) {
				staticStringBuilder.append( "FOLLOW("+ t._1 + ") = {" + t._2.mkString(",") + "}\r\n");
			}
		}
		val testMatrix1 = createMatrix()
		staticTestMatrix = testMatrix1;
		for( ex <- LL1_G ) {
			staticStringBuilder2.append( ex._1 + "->" + ex._2 + "\r\n")
		}
	}

	/*
	* Function name: GUI1
	* Function description: 实现图形化界面展示，开始界面
	* Input parameters: 无
	* Return value: 无
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sun Oct 20 2019 +080
	* Editor: 来自高山
	* Edited Date: Sun Oct 20 2019 +0800
	 */
	def GUI1(): Unit = {
		val jFrame = new JFrame("数据显示");
		val jPanel = new JPanel();
		jFrame.setBounds( 0, 10,1000,90);

		val appendFileJButton2 = new JButton("开始分析");
		appendFileJButton2.setBounds( 100, 400,200,30);
		appendFileJButton2.addActionListener(new ActionListener {
			override def actionPerformed(e: ActionEvent): Unit = {
				GUI2
				jFrame.dispose()
			}
		})

		//添加文件按钮
		val appendFileJButton = new JButton("添加文件");
		appendFileJButton.setBounds( 300, 400,200,30);
		appendFileJButton.addActionListener(new ActionListener {
			override def actionPerformed(e: ActionEvent): Unit = {
				val fileChooser = new JFileChooser();
				fileChooser.showOpenDialog(jFrame);
				val filePath = fileChooser.getSelectedFile.getAbsolutePath
				initiate(filePath)
				utility
			}
		})

		val appendFileJButton3 = new JButton("退出程序")
		appendFileJButton3.setBounds(500, 400, 200, 30)
		appendFileJButton3.addActionListener(new ActionListener {
			override def actionPerformed(e: ActionEvent): Unit = {
				jFrame.dispose()
			}
		})

		jPanel.add(appendFileJButton)
		jPanel.add(appendFileJButton2)
		jPanel.add(appendFileJButton3)
		jPanel.setBackground(Color.gray)
		jFrame.add(jPanel)
		import java.awt.FlowLayout
		jPanel.setLayout(new FlowLayout(FlowLayout.LEADING, 200, 20))
		jFrame.setResizable(false);
		jFrame.setVisible(true);
	}

	/*
	* Function name: GUI2
	* Function description: 实现图形化界面展示，分析界面
	* Input parameters: 无
	* Return value: 无
	* Exception: 未处理
	* Author: 菊花侠
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sun Oct 20 2019 +0800
	 */
	def GUI2(): Unit = {
		val jFrame = new JFrame("LL(1)词法分析");
		jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
		jFrame.setResizable(false)
		jFrame.setBounds(0,0,1000,810);
		jFrame.setLayout(null)

		//输入行
		val inputJPanel = new JPanel();
		inputJPanel.setBounds(0,0,1000,30);
		inputJPanel.setLayout(null);

		val inputJTextField = new JTextField();
		inputJTextField.setBounds(0,0,300,30);
		//    inputJTextField.setText("i+i*i*i#");
		//    inputJTextField.setPreferredSize(new Dimension(300,30));
		val inputJButton = new JButton("确认");
		inputJButton.setBounds(320,0,60,30);
		//    inputJButton.setPreferredSize(new Dimension(30,30));
		inputJPanel.add(inputJTextField)
		inputJPanel.add(inputJButton)
		jFrame.add(inputJPanel);


		val displayFileJTextPane = new JTextPane();
		displayFileJTextPane.setEditable(false);

		val displayFileJScrollPane = new JScrollPane();
		displayFileJScrollPane.setBounds(0,64,1000,300);
		displayFileJScrollPane.setViewportView(displayFileJTextPane);
		jFrame.add(displayFileJScrollPane)

		//添加文件按钮
		//		val appendFileJButton = new JButton("添加文件");
		val appendFileJButton = new JButton("显示当前文法")
		appendFileJButton.setBounds(0, 32,120,30)
		appendFileJButton.addActionListener(new ActionListener {
			override def actionPerformed(e: ActionEvent): Unit = {
				displayFileJTextPane.setText(staticStringBuilder2.toString());
			}
		})

		val appendFileJButton2 = new JButton("返回")
		appendFileJButton2.setBounds(220, 32, 120, 30)
		appendFileJButton2.addActionListener(new ActionListener {
			override def actionPerformed(e: ActionEvent): Unit = {
				GUI1
				jFrame.dispose()
			}
		})

		val appendFileJButton3 = new JButton("退出")
		appendFileJButton3.setBounds(440, 32, 120, 30)
		appendFileJButton3.addActionListener(new ActionListener {
			override def actionPerformed(e: ActionEvent): Unit = {
				jFrame.dispose()
			}
		})

		jFrame.add(appendFileJButton)
		jFrame.add(appendFileJButton2)
		jFrame.add(appendFileJButton3)

		val dataMode1 = new AbstractTableModel() {

			override def getColumnCount = 5

			override

			def getRowCount = staticAnalyseList.length

			override

			def getValueAt(row: Int, col: Int): String = {
				val a = staticAnalyseList(row);
				if(col == 0){
					return a.getStep();
				}
				if(col == 1){
					return a.getAnalysisStack;
				}
				if(col == 2){
					return a.getRemainingString;
				}
				if(col == 3){
					return a.getProductionType;
				}
				if(col == 4){
					return a.getAction;
				}
				return new String();
			}
		}

		val table1JScrollPane = new JScrollPane();
		val table1JTable = new JTable(dataMode1);
		table1JScrollPane.setBounds(0,370,690,300)
		//    table1JTable.setBounds(0,170,500,300);
		table1JScrollPane.setViewportView(table1JTable);
		val table1JTextPaneScrollPan = new JScrollPane();
		table1JTextPaneScrollPan.setBounds(692,370,300,300);
		val table1JTextPane = new JTextPane();
		table1JTextPane.setEditable(false);
		table1JTextPaneScrollPan.setViewportView(table1JTextPane);
		jFrame.add(table1JTextPaneScrollPan);
		jFrame.add(table1JScrollPane);

		val table2JScrollPane = new JScrollPane();
		table2JScrollPane.setBounds(0,682,300,300)
		//    table2JScrollPane.setBounds(0,482);

		val table2JTable = new JTable();
		table2JTable.setBounds(0,682,1000,300);
		val r = new DefaultTableCellRenderer();
		r.setHorizontalAlignment(0)
		table2JTable.setDefaultRenderer(classOf[Any],r);
		table2JScrollPane.add(table2JTable)
		jFrame.add(table2JTable);

		jFrame.setVisible(true);

		inputJButton.addActionListener(new ActionListener {
			override def actionPerformed(e: ActionEvent): Unit = {
				staticAnalyseList.clear();
				//        staticTestMatrix : Array[ Array[String] ] = new Array[Array[String]](0)
				staticTestMatrix = createMatrix();
				analyse( inputJTextField.getText() + "#" );
				val dataMode1 = new AbstractTableModel() {

					override def getColumnCount = 5

					override

					def getRowCount = staticAnalyseList.length

					override

					def getValueAt(row: Int, col: Int): String = {
						val a = staticAnalyseList(row);
						if(col == 0){
							return a.getStep();
						}
						if(col == 1){
							return a.getAnalysisStack;
						}
						if(col == 2){
							return a.getRemainingString;
						}
						if(col == 3){
							return a.getProductionType;
						}
						if(col == 4){
							return a.getAction;
						}
						return new String();
					}

				}
				table1JTable.setModel(dataMode1);

				val dataMode2 = new AbstractTableModel() {

					override def getColumnCount = 9

					override

					def getRowCount = staticTestMatrix.length

					override

					def getValueAt(row: Int, col: Int) = staticTestMatrix(row)(col)
				}
				table2JTable.setModel(dataMode2);
				table1JTextPane.setText(staticStringBuilder.toString())
			}
		})
	}

	/*
	* Function name: initiate
	* Function description: 初始化全局变量
	* Input parameters: the absolute path of the language-rule source file
	* Return value: 无
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def initiate( filePath: String ): Unit = {
		LL1_G = parseFile(filePath)
		allCharacters = getWholeCharacters(LL1_G)
		usedCharacters = allCharacters
		relations = getRelation(LL1_G)
		VN = getVN(allCharacters)
		VT = getVT(allCharacters)
		eliminateLeftRecursion      // eliminate all the left recursion at first
	}

	/*
	* Function name: displayRelations
	* Function description: display all he language rules
	* Input parameters: 无
	* Return value: 无
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
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

	/*
	* Function name: parseFile
	* Function description: 解析文本文件，保存在数组中
	* Input parameters: 文本绝对路径
	* Return value: -ArrayBuffer[ ( String, String ) ]（String类型的元组ArrayBuffer数组）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 18 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 18 2019 +0800
	 */
	def parseFile( filePath: String ): ArrayBuffer[ ( String, String ) ] = {
		val result = new ArrayBuffer[ ( String, String ) ]( countLines( readFromTxtByLine(filePath) ) )
		val sourceFile = readFromTxtByLine(filePath) //filePath
		for( line <- sourceFile ) {
			val tmp = line.split( "->", 2 )
			result += ( ( tmp.head, tmp.last ) )
		}
		result
	}

	/*
	* Function name: countLines
	* Function description: 计算文本行数，用于创建接收数组时开辟相应空间
	* Input parameters: -Array[String]（文本文件数据构成的数组）
	* Return value: -Int（文本行数）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 18 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def countLines( sourceFile: Array[String] ): Int = {
		var cnt = 0
		for( line <- sourceFile ) {
			cnt += 1
		}
		cnt
	}

	/*
	* Function name: readFromTxtByLine
	* Function description: 读取文本文件
	* Input parameters: -String（文本文件绝对路径）
	* Return value: -Array[String]（文本文件构成的数组，每行数据占一个数组元素）
	* Exception: -未处理
	* Author: 来自高山
	* Created date: Fri Oct 18 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 18 2019 +0800
	 */
	def readFromTxtByLine(filePath: String): Array[String] = {
		import scala.io.Source
		val source = Source.fromFile(filePath, "UTF-8")
		//val lineIterator = source.getLines()
		//lineIterator.foreach()
		val lines = source.getLines().toArray
		source.close()
		//println(lines.size)
		lines
	}

	/*
	* Function name: getWholeCharacters
	* Function description: 获取文法的除“|”之外的所有字符
	* Input parameters: -ArrayBuffer[ (String, String) ]（由文法左右两部分字符构成一个元组的数组，筛掉“|”）
	* Return value: -String（文法的除“|”之外的所有字符）
	* Exception: 未处理（有出错提示）
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
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

	/*
	* Function name: getVN
	* Function description: 获取文法的所有非终结符（non-terminal character），默认大写字母为非终结符，使用正则表达式匹配
	* Input parameters: -String（函数getWholeCharacters传来的文法的所有字符）
	* Return value: -String（文法的所有非终结符）
	* Exception: 未处理（有出错提示）
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
	def getVN( string: String ): String = {
		//match big letter:
		//^[A-Z]+$
		val pattern = new Regex("[A-Z]")//("^[A-Z]+$")
		if( (pattern findAllIn string) != null )
			(pattern findAllIn string).mkString("")
		else
			"function getVN failed"
	}

	/*
	* Function name: getVT
	* Function description: 获取文法的所有非终结符（terminal character），默认大写字母外的字符为终结符，使用正则表达式匹配
	* Input parameters: -String（函数getWholeCharacters传来的文法的所有字符）
	* Return value: -String（文法的所有终结符）
	* Exception: 未处理（有出错提示）
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
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

	/*
	* Function name: getRelation
	* Function description: 获取文法每一行对应的推导关系，若文法只推出了1项（即没有符号“|”），则返回元组的第三个用希伯来字母“א”示空
	* Input parameters: -ArrayBuffer[ (String, String)（已经分割好的文法左右部分构成的数组）
	* Return value: -ArrayBuffer[ (String, String, String) ]（元组第一个元素为推导式左边符号，第二为右边第二个符号串，第三为右边（若有）第三个符号串）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
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

	/*
	* Function name: findFirst
	* Function description: 获取指定字符的右边两个（可能是一个）导出字符串的首个非 ε 组成的字符串
	* Input parameters: -String（指定字符）
	* Return value: -String（指定字符的右边两个（可能是一个）导出字符串的首个非 ε 组成的字符串）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
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

	/*
	* Function name: judgeOnlyOneVoidSuccession
	* Function description: 判断指定字符是否可推出唯一的字符ε
	* Input parameters: -String（指定字符串）
	* Return value: -Boolean（存在则true，否则false）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Fri Oct 11 2019 +0800
	* Editor: 来自高山
	* Edited Date: Fri Oct 11 2019 +0800
	 */
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

	/*
	* Function name: judgeCaseXY
	* Function description: 判断构造FIRST集时可能的第3种情况的（1），即若X->Y...是一个产生式且Y∈VN（省略若干描述）
	* Input parameters: -Char（指定字符，即可能满足条件的产生式的左边字符）
	* Return value: -Boolean（满足则true，否则false）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 12 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 12 2019 +0800
	 */
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

	/*
	* Function name: findCase_Y_In_XY
	* Function description: 获取构造FIRST集时可能的第3种情况的（1），即若X->Y...是一个产生式且Y∈VN（省略若干描述）时的Y
	* Input parameters: -Char（指定字符，即可能满足条件的产生式的左边字符）
	* Return value: -String（Y构成的String字符串，无则为空）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 12 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 12 2019 +0800
	 */
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

	/*
	* Function name: findCase_Y_In_nY
	* Function description: 获取构造FIRST集时可能的第3种情况的（2）时的FIRST(Yi)中所有的非ε-元素（省略描述若干字）
	* Input parameters: -Char（指定字符，即可能满足条件的产生式的左边字符）
	* Return value: -String（FIRST(Yi)中所有的非ε-元素构成的String字符串，无则为空）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 12 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 12 2019 +0800
	 */
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
						// add the element belongs to tmp
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
						// otherwise, reset tmp as empty string
						else {
							tmp = ""
						}
					}
					if (cnt == ex._2.length) {
						result += tmp
					}

					// reset
					cnt = 0
					tmp = ""
					for (tx <- ex._3) {
						// add the element belongs to tmp
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
						// otherwise, reset result as empty string
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
						// add the element belongs to tmp
						if (localVN.contains(tx)) {
							tmp += tx.toString
							cnt += 1
						}
						// otherwise, reset tmp as empty string
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

	/*
	* Function name: FIRST
	* Function description: 按照教材P78左下角的算法描述实现求解指定文法FIRST集；因用的是循环迭代求解，因此代码较长
	* Input parameters: -ArrayBuffer[ (String, String) ]（产生式左右两部分分别构成元组的第1个和第2个元素）
	* Return value: -Map[ String, String ]（Map的key是非终结符，value是其FIRST元素）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Mon Oct 14 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def FIRST( string: ArrayBuffer[ (String, String) ] ): Map[ String, String ] = {
		val FIRST_Group = Map[ String, String ]()

		val wholeCharacters = allCharacters
		val localVT = VT
		val localVN = VN

		for( character <- wholeCharacters ) {
			// case 1
			if( localVT.contains(character) ) {
				//if there exist the original key that equals the current one
				if( FIRST_Group.contains(character.toString) == true ) {
					val tmp = character.toString + FIRST_Group(character.toString)
					FIRST_Group(character.toString) = tmp.distinct
				}
				//otherwise
				else {
					FIRST_Group(character.toString) = character.toString
				}
			}

			// case 2
			if( localVN.contains(character.toString) == true ) {
				// case 2.1
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

				// case 2.2
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
				// case 3
				// case 3.1
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

				// case 3.2
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
				// case 3.3
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

	/*
	* Function name: FOLLOW
	* Function description: 根据dfsFOLLOW函数，获取各个非终结符的FOLLOW集元素
	* Input parameters: -ArrayBuffer[ (String, String) ]（产生式左右两部分分别构成元组的第1个和第2个元素）
	* Return value: -Map[ String, String ]（Map的key是非终结符，value是其FOLLOW集元素）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def FOLLOW( string: ArrayBuffer[ (String, String) ] ): Map[ String, String ] = {
		val localVN = VN
		val FOLLOW_Group = Map[ String, String ]()
		for( ch <- localVN ) {
			FOLLOW_Group(ch.toString) = dfsFOLLOW(ch.toString)
		}
		FOLLOW_Group
	}

	/*
	* Function name: dfsFOLLOW
	* Function description: 使用深度优先搜索（DFS）寻找各个非终结符的FOLLOW集元素
	* Input parameters: -String（指定的非终结符）
	* Return value: -String（指定终结符的FOLLOW集元素）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
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

	/*
	* Function name: findGivenValueFOLLOWPosition
	* Function description: 按照教材P79右上角的算法描述，求解构成每个非终结符的FOLLOW集的“依赖”（因为实现了这个函数，节省了我原先用循环叠加求解FOLLOW集的700+代码）
	* Input parameters: -String（指定终结符）
	* Return value: -String（指定终结符的FOLLOW集元素，无则为空）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def findGivenValueFOLLOWPosition( ch: String ): String = {
		var result = ""
		val cnt = new ArrayBuffer[String]()
		val localRelations = relations

		for( ex <- localRelations ) {
			if( ex._3 != "א" ) {
				if( ex._2.contains(ch) ) {
					// מ
					if( ex._2.length == 3 ) {
						// B                                    A       α                 B         β
						if( ex._2(1).toString == ch && judgeCase2( ex._1, ex._2(0).toString, ch, ex._2(2).toString ) ) {
							val value = ex._2(2).toString + "T"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
						// B                                    A       α                 B         β
						if( ex._2(1).toString == ch && judgeCase3( ex._1, ex._2(0).toString, ch, ex._2(2).toString ) ) {
							val value = ex._1.toString + "W"
							if( cnt.contains(value) == false ) {
								cnt += value
								result += value
							}
						}
					}
					if( ex._2.length == 2 ) {
						// B                                    A       α                 B
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
						// B                                      A       α                 B         β
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
						// B                                    A       α                 B
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
						// B                                      A       α              B         β
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
						// B                                    A       α                 B
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

	/*
	* Function name: judgeCase2
	* Function description: 按照教材P79右下角的算法描述，判断是否填充满足条件（2）的矩阵元素
	* Input parameters: -String, String, String, String[分别代表条件（2）的四个字符]
	* Return value: -Boolean（满足条件（2）则返回true，否则返回false）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Tue Oct 15 2019 +0800
	* Editor: 来自高山
	* Edited Date: Tue Oct 15 2019 +0800
	 */
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

	/*
	* Function name: judgeCase3
	* Function description: 按照教材P79右下角的算法描述，判断是否填充满足条件（3）的矩阵元素
	* Input parameters: -String, String, String, String[分别代表条件（3）的四个字符]
	* Return value: -Boolean（满足条件（3）则返回true，否则返回false）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Wed Oct 16 2019 +0800
	* Editor: 来自高山
	* Edited Date: Wed Oct 16 2019 +0800
	 */
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

	/*
	* Function name: initiateMatrix
	* Function description: 初始化分析表（为了在控制台打印方便，表长为非终结符个数加一，表宽为终结符个数加一）
	* Input parameters: 无
	* Return value: -Array[ Array[ String] ]（分析表矩阵元素构成的二维数组，除了第0行和第0列，其它列与行的元素均为null）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Wed Oct 16 2019 +0800
	* Editor: 来自高山
	* Edited Date: Wed Oct 16 2019 +0800
	 */
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

	/*
	* Function name: createMatrix
	* Function description: 按照教材P79右下角的算法描述，构造分析表
	* Input parameters: 无
	* Return value: -Array[ Array[String] ]（分析表矩阵元素构成的二维数组）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Wed Oct 16 2019 +0800
	* Editor: 来自高山
	* Edited Date: Wed Oct 16 2019 +0800
	 */
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

					// case 3
					if( range.contains("ε") == true && flag == false ) {
						for( b <- localFOLLOW(α) ) {
							result( getRow(ex._1.toString) )( getColumn(b.toString) ) = ex._1 + "->" + ex._2  // t --> tx
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

					// case 3
					if( range.contains("ε") == true && flag == false ) {
						for( b <- localFOLLOW(ex._1) ) {
							result( getRow(ex._1.toString) )( getColumn(b.toString) ) = ex._1 + "->" + ex._3  // t --> tx
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

					// case 3
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

	/*
	* Function name: getRow
	* Function description: 获取指定字符在分析表中的行数
	* Input parameters: -String（指定字符）
	* Return value: -Int（指定字符所在的行数）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Wed Oct 16 2019 +0800
	* Editor: 来自高山
	* Edited Date: Wed Oct 16 2019 +0800
	 */
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

	/*
	* Function name: getColumn
	* Function description: 获取指定字符在分析表中的列数
	* Input parameters: -String（指定字符）
	* Return value: -Int（指定字符所在的列数）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Wed Oct 16 2019 +0800
	* Editor: 来自高山
	* Edited Date: Wed Oct 16 2019 +0800
	 */
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

	/*
	* Function name: analyse
	* Function description: 对指定的字符串进行LL(1)分析
	* Input parameters: -String（输入的指定字符串）
	* Return value: -Boolean（分析成功则返回true，否则false）
	* Exception: 未处理（有出错提示）
	* Author: 来自高山
	* Created date: Wed Oct 16 2019 +0800
	* Editor: 来自高山
	* Edited Date: Wed Oct 16 2019 +0800
	 */
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
		staticAnalyseList.append(new Analyse("步骤","分析栈","剩余字符串","所用产生式","动作"));
		staticAnalyseList.append(new Analyse(cnt.toString, displayStack(stack).reverse.toString,localExpression.toString,"","initiate"));
		while( stack.isEmpty == false ) {
			val stackTop = stack.top
			stack.pop()
			// 栈顶符号属于  非终结符
			if( localVN.contains(stackTop) == true ) {
				// 栈顶符号与表达式左端首字符  存在  关系
				if( table( getRow(stackTop) )( getColumn( localExpression(0).toString ) ) != null ) {
					val lastHalf = table( getRow(stackTop) )( getColumn( localExpression(0).toString ) ).split( "->", 2 ).last
					val length = lastHalf.length
					for( i <- 0 to (length - 1) ) {
						if( lastHalf != "ε" ) {
							stack.push(lastHalf(length - 1 - i).toString)
						}
					}
					cnt += 1

					if( lastHalf != "ε" ) {
						staticAnalyseList.append(new Analyse(cnt.toString, displayStack(stack).reverse.toString, localExpression.toString, table(getRow(stackTop))(getColumn(localExpression(0).toString)), "POP, PUSH(" + lastHalf.reverse + ")"));
					}
					else {
						staticAnalyseList.append(new Analyse(cnt.toString, displayStack(stack).reverse.toString, localExpression.toString, table(getRow(stackTop))(getColumn(localExpression(0).toString)), "POP"));
					}
				}
				// 栈顶符号与表达式左端首字符  不存在  关系
				else {
					// 栈顶符号 等于 表达式左端首字符
					if( stackTop == "#" && localExpression(0).toString == "#" ) {
						println("11111")
						return true
					}
					// 栈顶符号 不等于 表达式左端首字符
					else {
						println("1 - error")
						staticAnalyseList.append(new Analyse(cnt.toString, displayStack(stack).reverse.toString,localExpression.toString,"",""));
						return false
					}
				}
			}
			// 栈顶符号属于  终结符
			if( localVT.contains(stackTop) == true ) {
				// 栈顶符号 等于 表达式左端首字符
				if( stackTop == localExpression(0).toString ) {
					if( stackTop == localExpression(0).toString ) {
						//stack.pop()
						localExpression = localExpression.drop(1)
						cnt += 1
						staticAnalyseList.append(new Analyse(cnt.toString, displayStack(stack).reverse.toString,localExpression.toString,"","GETNEXT(" + stackTop + ")"));
					}
					// 栈顶符号 不等于 表达式左端首字符
					else {
						println("2 - error")
						return false
					}
				}
			}
		}

		true
	}

	/*
	* Function name: judgeLeftRecursion
	* Function description: 判断是否存在形式上的左递归
	* Input parameters: -(String, String, String)（产生式的左端与右端的两个（或为1个）元素）
	* Return value: -Int（0表示无，1表示右端第1个元素存在形式上的左递归，2表示右端第2个元素）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def judgeLeftRecursion( expression: (String, String, String) ): Int = {
		var ans = 0             // ans = 0 means the current expression is not involved left-recursion
		if( expression._2.length >= 2 && expression._1 == expression._2(0).toString && expression._2.drop(1) != "ε" ) {
			ans += 1            // ans = 1 means the left recursion involves the expression._2
		}
		if( expression._3.length >= 2 && expression._1 == expression._3(0).toString && expression._3.drop(1) != "ε" ) {
			ans += 2            // ans = 2 means the left recursion involves the expression._3
		}
		ans                     // ans = 3 means the given expression is false since both exp(2) and exp(3) involved
	}

	/*
	* Function name: eliminateLeftRecursion
	* Function description: 消除形式上的左递归
	* Input parameters: 无
	* Return value: -ArrayBuffer[ (String, String, String) ]（消除左递归后的新文法）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
	def eliminateLeftRecursion(): ArrayBuffer[ (String, String, String) ] = {
		var localRelations = relations
		var invalidRelations = new ArrayBuffer[ (String, String, String) ]()
		val localCandidateLetters = allCandidateLetters
		val VN1 = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩABCDEFGHIJKLMNOPQRSTUVWXYZ"
		val VT1 = "αβγδεζηθικλμνξοπρστυφχψωabcdefghijklmnopqrstuvwxyz"
		for( ex <- localRelations ) {
			if( ex._3 != "א" ) {
				if( judgeLeftRecursion( ex._1, ex._2, ex._3 ) == 1 ) {
					// P = ex._1, α = ex._2 - ex._1, β = ex._3,  P' = newValue
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
					//					println( "1 -- exp1._1 = " + exp1._1 + ", exp1._2 = " + exp1._2 + ", exp1._3 = " + exp1._3 )
					//					println( "1 -- exp2._1 = " + exp2._1 + ", exp2._2 = " + exp2._2 + ", exp2._3 = " + exp2._3 )
					localRelations += exp1
					localRelations += exp2
				}
				else if( judgeLeftRecursion( ex._1, ex._2, ex._3 ) == 2 ) {
					// P = ex._1, α = ex._3 - ex._1, β = ex._3,  P' = newValue
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
					//					println( "2 -- exp1._1 = " + exp1._1 + ", exp1._2 = " + exp1._2 + ", exp1._3 = " + exp1._3 )
					//					println( "2 -- exp2._1 = " + exp2._1 + ", exp2._2 = " + exp2._2 + ", exp2._3 = " + exp2._3 )
					localRelations += exp1
					localRelations += exp2
				}
				else if( judgeLeftRecursion( ex._1, ex._2, ex._3 ) == 3 ){
					println( "error in the function eliminateLeftRecursion" )
				}
			}
			else {
				if( judgeLeftRecursion( ex._1, ex._2, ex._3 ) == 1 ) {
					// P = ex._1, α = ex._2 - ex._1, β = ex._3,  P' = newValue
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
					//					println( "3 -- exp1._1 = " + exp1._1 + ", exp1._2 = " + exp1._2 + ", exp1._3 = " + exp1._3 )
					//					println( "3 -- exp2._1 = " + exp2._1 + ", exp2._2 = " + exp2._2 + ", exp2._3 = " + exp2._3 )
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

	/*
	* Function name: subString
	* Function description: 获取两输入字符串的差集（要求两者均非空）
	* Input parameters: 无
	* Return value: -String（两输入字符串的差集）
	* Exception: 未处理
	* Author: 来自高山
	* Created date: Sat Oct 19 2019 +0800
	* Editor: 来自高山
	* Edited Date: Sat Oct 19 2019 +0800
	 */
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
