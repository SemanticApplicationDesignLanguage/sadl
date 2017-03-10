package com.ge.research.sadl.testjena;

import static org.junit.Assert.*;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Before;
import org.junit.Test;

import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.graph.NodeFactory;
import com.hp.hpl.jena.reasoner.rulesys.BindingEnvironment;

public class RegexPatternMatcher {

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void test() {
		String text = "EM 245";
//		String pattern = "EM(\\s|.)*";
		String pattern = "EM";
        Matcher m = Pattern.compile(pattern).matcher(text);
        if ( m.matches()) {
        	int numGroups = m.groupCount();
            // bind any capture groups
            for (int i = 0; i < numGroups; i++) {
                String gm = m.group(i+1);
                System.out.println(gm);
             }
        }
        else {
        	System.out.println("no matches");
        }
	}

	@Test
	public void regexTest() { 
		String INPUT_STRING = "EM 245"; 
        if (INPUT_STRING.matches("^EM.*$")) { 
            String[] splitString = INPUT_STRING.split("EM", 2); 
            assertEquals(2, splitString.length); 
            assertTrue(splitString[0].isEmpty()); 
            assertEquals(" 245", splitString[1]); 
        } else { 
            fail("String did not start with EM"); 
        } 
	}
}
