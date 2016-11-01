package com.ge.research.sadl.jena.inference;

import static org.junit.Assert.*;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import org.junit.Test;

public class TestJsEval {

	@Test
	public void test() throws ScriptException {
		ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
		engine.eval("print('Hello World!');");
		engine.eval("print(1+2+3);");
		engine.eval("function summy(x,y) {sum=x+y; return sum;}"
				+ "print(summy(6,7));");
	}

}
