/**
* parser.scala - Parser implementation
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

import scala.collection.mutable.Map

class Parser (strat : CalculationStrategy) {
	val strategy : CalculationStrategy = strat;

	var constEnvironment : Map[String,ValueConst] = Map();
	var chanEnvironment : Map[String, Channel] = Map(); 

	def resetConstEnvironment() = {
		constEnvironment = Map();
	}

	def removeParenthesis(str : String) : String = {
		if(str.startsWith("(") && str.endsWith(")")) {
			return str.substring(1, str.length()-1);
		}
		else if(str.startsWith("(")) {
			return str.substring(1, str.length());
		}
		return str;
	}

	def parseStrPar(str : String, find : String, init : Int) : Int = {
		var parenthesisCount = init;
		var n = str.length();
		var i = 0;
		while((parenthesisCount != 0 || !str.substring(i).startsWith(find)) && i < n) {
			if (str.charAt(i) == '(')
				parenthesisCount += 1;
			else if (str.charAt(i) == ')')
				parenthesisCount -= 1;
			i += 1;
		}
		if (i < n)
			return i;
		return -1;
	}

	def removeProcParenthesis(str : String) : String = {
		val i = parseStrPar(str, ")", -1);
		return (str.substring(1, i) + str.substring(i+1));
	}

	def isAList(str : String) : Boolean = {
	    var parenthesisCount = 0; 
	    var i = 0;
	    val lim = str.length();

	    while(i < lim - 1 
	    	&& (parenthesisCount != 0 || str.charAt(i) != ':' || str.charAt(i+1) != ':') 
	    	&& (parenthesisCount != 0 || str.charAt(i) != '[' || str.charAt(i+1) != ']')) {
	        if (str.charAt(i) == '(')
	            parenthesisCount += 1;
	        else if (str.charAt(i) == ')')
	            parenthesisCount -= 1;
	        i += 1;
	    }
	    return i != (lim-1);
	}

	def splitList(str:String) : List[String] = {
		var parenthesisCount = 0;
		var list : List[String] = List();
		var precTerm = 0;
		
		for(i <- 0 to str.length()-2) {
			if(parenthesisCount == 0 && str.charAt(i) == ':' && str.charAt(i+1) == ':') {
				list = (str.substring(precTerm, i))::list;
				precTerm = i+2;
			}
			if(str.charAt(i) == '(')
				parenthesisCount += 1;
			else if(str.charAt(i) == ')')
				parenthesisCount -= 1;
		}

		list = (str.substring(precTerm))::list;

		return list.reverse;
	}

	def parseValue(str: String) : Value = {
		if (str.charAt(0) == '(' && parseStrPar(str, ")", -1) == str.length()-1) 
			return parseValue(removeParenthesis(str));

		if (str.matches("[0-9]+")) {
			return new ValueInteger(str.toInt);
		}
		else if(str.startsWith("not(")) {
			val valueEnd = str.lastIndexOf(')');
			val res = parseValue(str.substring(4, valueEnd));
			return new ValueNot(res);
		}
		else if(parseStrPar(str, ">", 0) != -1) {
			val cut = parseStrPar(str, ">", 0);
			val leftV = parseValue(str.substring(0,cut));
			val rightV = parseValue(str.substring(cut+1,str.length()));
			return new ValueSuperior(leftV, rightV);
		}
		else if(parseStrPar(str, "=", 0) != -1) {
			val cut = parseStrPar(str, "=", 0);
			val leftV = parseTerm(str.substring(0,cut));
			val rightV = parseTerm(str.substring(cut+1,str.length()));
			return new ValueEqual(leftV, rightV);
		}
		else if(parseStrPar(str, "/\\", 0) != -1) {
			val cut = parseStrPar(str, "/\\", 0);
			val leftV = parseValue(str.substring(0,cut));
			val rightV = parseValue(str.substring(cut+2,str.length()));
			return new ValueAnd(leftV, rightV);
		}
		else if(parseStrPar(str, "\\/", 0) != -1) {
			val cut = parseStrPar(str, "\\/", 0);
			val leftV = parseValue(str.substring(0,cut));
			val rightV = parseValue(str.substring(cut+2,str.length()));
			return new ValueOr(leftV, rightV);
		}
		else if(str.startsWith("count(")) {
			val valueEnd = str.lastIndexOf(')');
			val list = parseTerm(str.substring(6, valueEnd));
			list match {
				case TermList(l) => return new ValueCount(list);
				case TermVariable(s) => return new ValueCount(list);
				case _ => throw new parsingError("count() need a list or a variable parameter");
			}
		}
		else if(str.startsWith("count")) {
			val list = parseTerm(str.substring(5));
			list match {
				case TermList(l) => return new ValueCount(list);
				case TermVariable(s) => return new ValueCount(list);
				case _ => throw new parsingError("count() need a list or a variable parameter");
			}
		}
		else {
			try {
				return constEnvironment(str)
			}
			catch {
				case _ : Throwable => {
					val exc = str + " is not a value. Maybe you should try to define it with new";
					throw new parsingError(exc);
				}
			}
		}
	}

	def parseTermList(str : List[String]) : List[Term] = {
		str match {
			case Nil => throw new parsingError("List syntax error");
			case s :: Nil if s == "[]" => return List()
			case "[]" :: tail => throw new parsingError("List syntax error");
			case s :: tail => {
				val term = parseTerm(s);
				val list = parseTermList(tail);
				return term::list;
			}
		}
	}

	def parseTerm(str: String) : Term = {
		if (str.startsWith("(")) {
			return parseTerm(removeParenthesis(str));
		}

		if (str.startsWith("pair(")) {
			val nextComma = parseStrPar(str, ",", -1);
			val termEnd = str.lastIndexOf(')');
			val leftTerm = parseTerm(str.substring(5, nextComma));
			val rightTerm = parseTerm(str.substring(nextComma+1, termEnd));
			return new TermPair(leftTerm, rightTerm);
		}
		else if (str.startsWith("pi1(")) {
			val termEnd = str.lastIndexOf(')');
			val termProj = parseTerm(str.substring(4, termEnd))
			return new TermProj1(termProj);
		}
		else if (str.startsWith("pi2(")) {
			val termEnd = str.lastIndexOf(')');
			val termProj = parseTerm(str.substring(4, termEnd))
			return new TermProj2(termProj);
		}
		else if (str.startsWith("enc(")) {
			val nextComma = parseStrPar(str, ",", -1);
			val termEnd = str.lastIndexOf(')');
			val leftTerm = parseTerm(str.substring(4, nextComma));
			val rightTerm = parseTerm(str.substring(nextComma+1, termEnd));
			return new TermEncode(leftTerm, rightTerm);
		}
		else if (str.startsWith("dec(")) {
			val nextComma = parseStrPar(str, ",", -1);
			val termEnd = str.lastIndexOf(')');
			val leftTerm = parseTerm(str.substring(4, nextComma));
			val rightTerm = parseTerm(str.substring(nextComma+1, termEnd));
			return new TermDecode(leftTerm, rightTerm);
		}
		else if (str.startsWith("pk(")) {
			val termEnd = str.lastIndexOf(')');
			val valueKey = parseValue(str.substring(3, termEnd));
			return new TermPublicKey(valueKey);
		}
		else if (str.startsWith("sk(")) {
			val termEnd = str.lastIndexOf(')');
			val valueKey = parseValue(str.substring(3, termEnd));
			return new TermSecretKey(valueKey);
		}
		else if (isAList(str)) {
			if(str.indexOf("::") != -1) {
				val res = parseTermList(splitList(str));
				return new TermList(res);
			}
			else {
				return new TermList(Nil);
			}
		}
		else {
			try {
				val value = parseValue(str);
				return value;
			}
			catch {
				case _ : Throwable => return new TermVariable(str);
			}
		}
	}

	def parseProc(str : String) : Proc = {
		if (str.startsWith("(")) {
			return parseProc(removeProcParenthesis(str));
		}

		if (str.startsWith("in(")) {
			val nextComma = str.indexOf(',');
			val procEnd = str.indexOf(").");
			val channelName = str.substring(3, nextComma);
			val variable = str.substring(nextComma+1, procEnd);
			val nextProc = parseProc(str.substring(procEnd+2));
			try {
				return new ProcIn(chanEnvironment(channelName), variable, nextProc);
			}
			catch {
				case _ : Throwable => {
					val channel = new Channel(channelName, strategy);
					chanEnvironment.put(channelName, channel);
					return new ProcIn(channel, variable, nextProc);
				}
			}
		}
		else if (str.startsWith("in^")) { // Parsing in^k(c, x -> u as y)
			val begin = str.indexOf('('); // Positions of delimiters ( , -> ). and "as"
			val nextComma = str.indexOf(',');
			val procEnd = str.indexOf(").");
			val posAs = parseStrPar(str, "as", -1);
			val posArrow = parseStrPar(str, "->", -1);

			val number = str.substring(3, begin).toInt; // Parsing k
			val channelName = str.substring(begin+1, nextComma); // Parsing c
			val functionArg = str.substring(nextComma+1, posArrow); // Parsing x
			val functionRes = parseTerm(str.substring(posArrow+2, posAs)); // Parsing u
			val variable = str.substring(posAs+2,procEnd); //Parsing y

			val nextProc = parseProc(str.substring(procEnd+2));
			try {
				return new ProcInK(number, chanEnvironment(channelName), functionArg, functionRes, variable, nextProc);
			}
			catch {
				case _ : Throwable => {
					val channel = new Channel(channelName, strategy);
					chanEnvironment.put(channelName, channel);
					return new ProcInK(number, channel, functionArg, functionRes, variable, nextProc);
				}
			}
		}
		else if (str.startsWith("out(")) {
			val nextComma = str.indexOf(',');
			val procEnd = str.indexOf(").");
			val channelName = str.substring(4, nextComma);
			val message = parseTerm(str.substring(nextComma+1, procEnd));
			val nextProc = parseProc(str.substring(procEnd+2));
			try {
				return new ProcOut(chanEnvironment(channelName), message, nextProc);
			}
			catch {
				case _ : Throwable => {
					val channel = new Channel(channelName, strategy);
					chanEnvironment.put(channelName, channel);
					return new ProcOut(channel, message, nextProc);
				}
			}
		}
		else if (str.startsWith("if")) {
			val thenPos = str.indexOf("then");
			val elsePos = str.lastIndexOf("else");
			val valueTest = parseValue(str.substring(2, thenPos));
			val procThen = parseProc(str.substring(thenPos+4, elsePos)+".0");
			val procElse = parseProc(str.substring(elsePos+4)+".0");
			return new ProcIf(valueTest, procThen, procElse)
		}
		else if (str.startsWith("new")) {
			val endNew = str.indexOf('.');
			val nomVar = new ValueConst(str.substring(3, endNew));
			constEnvironment.put(str.substring(3,endNew), nomVar);
			val nextProc = parseProc(str.substring(endNew + 1));
			return new ProcNew(nomVar, nextProc);
		}
		else if (str.startsWith("0")) {
			return new ProcZero();
		}
		else {
			throw new parsingError("Program has to start with a process");
		}
	}

	def parseCopy(str : String) : (String, Int) = {
		val power = str.lastIndexOf('^');
		val res = str.substring(power+1);
		try {
			return (str.substring(0, power), res.toInt);
		}
		catch {
			case e:Exception => throw new parsingError("Power must be an integer");
		}
	}

	def copyProc(str : String, copies : Int) : List[String] = {
		var res : List[String] = Nil;
		for(i <- 1 to copies) {
			res = str::res
		}
		return res;
	}

	
	def parse(str : String) : List[Proc] = {
		val strArray = str.replaceAll(" ", "").replaceAll("\n", "").replaceAll("\t", "").split("""\|\|""");
		def parseAux (lstr : List[String]) : List[Proc]  = {
			resetConstEnvironment();
			lstr match {
				case List() => List()
				case p::tail if p.matches(".*\\^[0-9]+") => {
					val (p2, copies) = parseCopy(p);
					parseAux(copyProc(p2,copies):::tail);
				}
				case p::tail => parseProc(p.concat(".0"))::parseAux(tail)
			}
		}
		parseAux(strArray.toList);
	}

	def parseFile(path : String) : List[Proc] = {
		return parse(scala.io.Source.fromFile(path).mkString);
	}
}
