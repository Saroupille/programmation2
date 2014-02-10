/**
* parser.scala - Parser implementation
* @author Lanvin Victor Thiré François
* Copyright (c) 2014 GPLv3. See LICENCE file
*/

class Parser {
	def parseComma(str : String) : Int = {
	    //parenthesisCount = 0 ssi on a vu autant de parenthèses ouvrantes que fermantes (modulo la première ouvrante)
	    var parenthesisCount = -1;    // Initialisé à -1 car on va parser la parenthèse ouvrante de Pair(
	    var i = 0;
	    while(parenthesisCount != 0 || str.charAt(i) != ',') {
	        if (str.charAt(i) == '(')
	            parenthesisCount += 1;
	        else if (str.charAt(i) == ')')
	            parenthesisCount -= 1;
	        i += 1;
	    }
	    return i
	}

	def parseValue(str: String) : Value = {
		val value = str.toInt;
		return new ValueInteger(value);
	}

	def parseTerm(str: String) : Term = {
		if (str.startsWith("pair(")) {
			val nextComma = parseComma(str);
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
			val nextComma = parseComma(str);
			val termEnd = str.lastIndexOf(')');
			val leftTerm = parseTerm(str.substring(4, nextComma));
			val rightTerm = parseTerm(str.substring(nextComma+1, termEnd));
			return new TermEncode(leftTerm, rightTerm);
		}
		else if (str.startsWith("dec(")) {
			val nextComma = parseComma(str);
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
		else {
			try {
				val value = parseValue(str);
				return value;
			}
			catch {
				case _ => return new TermVariable(str);
			}
		}
	}

	def parseProc(str : String) : Proc = {
		if (str.startsWith("in(")) {
			val nextComma = str.indexOf(',');
			val procEnd = str.indexOf(").");
			val channel = str.substring(3, nextComma);
			val varName = parseTerm(str.substring(nextComma+1, procEnd));
			val nextProc = parseProc(str.substring(procEnd+2));
			return new ProcIn(channel, varName, nextProc);
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
			val procThen = parseProc(str.substring(thenPos+4, elsePos));
			val procElse = parseProc(str.substring(elsePos+4));
			return new ProcIf(valueTest, procThen, procElse)
		}
		else if (str.startsWith("new")) {
			val endNew = str.indexOf('.');
			val nomVar = new ValueConst(str.substring(3, endNew));
			val nextProc = parseProc(str.substring(endNew + 1));
			return new ProcNew(nomVar, nextProc);
		}
		else if (str.startsWith("0")) {
			return new ProcZero();
		}
		else {
			throw new parsingError("Fail");
		}
	}
	
	def parse(str : String) : List[Proc] = {
		val strArray = str.replaceAll(" ", "").split("""\|\|""");
		def parseAux (lstr : List[String]) : List[Proc]  = {
			lstr match {
				case List() => List()
				case p::tail => parseProc(p.concat(".0"))::parseAux(tail)
			}
		}
		parseAux(strArray.toList);
	}
}