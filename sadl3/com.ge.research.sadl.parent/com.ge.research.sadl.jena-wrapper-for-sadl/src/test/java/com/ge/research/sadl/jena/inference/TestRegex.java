package com.ge.research.sadl.jena.inference;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;

public class TestRegex {

	@Test
	public void test() {
		String text = "eo_isolating_eqpt_inst_1193929";
		String pattern = ".*inst_(.*)";
        Matcher m = Pattern.compile(pattern).matcher(text);
        if (m.matches()) {
        	int length = 2;
            // bind any capture groups
            for (int i = 0; i < m.groupCount(); i++) {
                String gm = m.group(i+1);
                System.out.println(gm);
             }
        }

	}

}
