/**
* parser.scala - Parser implementation
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

class Parser {
	var constEnvironment : List[(String, ValueConst)] = List();

	def resetEnvironment() = {
		constEnvironment = List();
	}

	def addToEnvironment(str: String, v : ValueConst) = {
		constEnvironment = (str,v)::constEnvironment;
	}

	def isInEnvironment(str:String) : Boolean = {
		def fun_aux(str:String, l:List[(String, ValueConst)]) : Boolean = {
			l match {
				case List() => false
				case (h,v)::t => (h == str) || fun_aux(str,t)
			}
		}
		fun_aux(str,constEnvironment);
	}

	def getOfEnvironment(str:String) : ValueConst = {
		def fun_aux(str:String, l:List[(String, ValueConst)]) : ValueConst = {
			l match {
				case List() => throw new parsingError("Unexpected error");
				case (h,v)::t => if (h == str) {return v;} else {fun_aux(str,t);}
			}
		}
		fun_aux(str,constEnvironment);
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
		else if(str.startsWith("count(")) {
			val valueEnd = str.lastIndexOf(')');
			val list = parseTerm(str.substring(6, valueEnd));
			list match {
				case TermList(l) => return new ValueCount(list);
				case TermVariable(s) => return new ValueCount(list);
				case _ => throw new parsingError("count() need a list or a variable parameter");
			}
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
		else if(isInEnvironment(str)) {
			return getOfEnvironment(str);
		}
		else {
			val exc = str + " is not a value. Maybe you should try to define it with new";
			throw new parsingError(exc);
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
			val channel = str.substring(3, nextComma);
			val varName = parseTerm(str.substring(nextComma+1, procEnd));
			val nextProc = parseProc(str.substring(procEnd+2));
			return new ProcIn(channel, varName, nextProc);
		}
		else if (str.startsWith("in^")) {
			val begin = str.indexOf('(');
			val nextComma = str.indexOf(',');
			val procEnd = str.indexOf(").");
			val number = str.substring(3, begin).toInt;
			val channel = str.substring(begin+1, nextComma);
			val function = str.substring(nextComma+1, procEnd);
			val nextProc = parseProc(str.substring(procEnd+2));
			return new ProcInK(number, channel, function, nextProc);
		}
		else if (str.startsWith("out(")) {
			val nextComma = str.indexOf(',');
			val procEnd = str.indexOf(").");
			val channel = str.substring(4, nextComma);
			val message = parseTerm(str.substring(nextComma+1, procEnd));
			val nextProc = parseProc(str.substring(procEnd+2));
			return new ProcOut(channel, message, nextProc);
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
			addToEnvironment(str.substring(3,endNew), nomVar);
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
			resetEnvironment();
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
