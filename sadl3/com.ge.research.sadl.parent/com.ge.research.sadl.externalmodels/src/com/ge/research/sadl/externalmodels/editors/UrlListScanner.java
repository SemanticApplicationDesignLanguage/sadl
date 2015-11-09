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
