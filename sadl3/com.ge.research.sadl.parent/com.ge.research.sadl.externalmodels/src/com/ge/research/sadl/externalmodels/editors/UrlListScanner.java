/************************************************************************
 * Copyright © 2007-2016 - General Electric Company, All Rights Reserved
 *
 * Project: SADL
 *
 * Description: The Semantic Application Design Language (SADL) is a
 * language for building semantic models and expressing rules that
 * capture additional domain knowledge. The SADL-IDE (integrated
 * development environment) is a set of Eclipse plug-ins that
 * support the editing and testing of semantic models using the
 * SADL language.
 *
 * This software is distributed "AS-IS" without ANY WARRANTIES
 * and licensed under the Eclipse Public License - v 1.0
 * which is available at http://www.eclipse.org/org/documents/epl-v10.php
 *
 ***********************************************************************/
package com.ge.research.sadl.externalmodels.editors;

import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Display;

public class UrlListScanner extends RuleBasedScanner {
	private static Color TAG_COLOR = new Color(Display.getCurrent(),new RGB(200,0,0));
	private static Color COMMENT_COLOR = new Color(Display.getCurrent(), new RGB(0,200,0));
	
	public UrlListScanner() {
		IToken tagToken = new Token(new TextAttribute(TAG_COLOR));
		IToken commentToken = new Token (new TextAttribute(COMMENT_COLOR));
		
		IRule[] rules = new IRule[2];
		rules[0] = new SingleLineRule("<",">",tagToken);
		rules[1] = (new EndOfLineRule("--", commentToken));
		setRules(rules);
	}
}
