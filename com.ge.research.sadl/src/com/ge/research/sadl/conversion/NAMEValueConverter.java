package com.ge.research.sadl.conversion;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.xtext.conversion.ValueConverterException;
import org.eclipse.xtext.conversion.impl.IDValueConverter;
import org.eclipse.xtext.nodemodel.ICompositeNode;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.INode;

import com.ge.research.sadl.builder.SadlModelManager;
import com.google.common.collect.Lists;

public class NAMEValueConverter extends IDValueConverter {
	protected Set<Character> collectInvalidCharacters(String value) {

		Set<Character> result = new HashSet<Character>();
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
				continue;
			if (i != 0 && c >= '0' && c <= '9')
				continue;

			if (c == '%' && value.length()>=i+2) {
				i++;
				c = value.charAt(i);
				if (isHexDigit(c)) {
					i++;
					c = value.charAt(i);
					if (isHexDigit(c)) {
						continue;
					}
				}
				result.add(c);
			}
			switch (c) {
				case '-':
				case '_':
				case '.':
				case '~':
					continue;
				default:
					result.add(c);
			}
		}
		return result;
	}

	private boolean isHexDigit (char c) {
		return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
	}


	@Override
	public String toValue(String string, INode node) {
		if (string == null)
			return null;
		// any instances of "^" are there as escape characters and should be removed
		String value = SadlModelManager.removeEscapeCharacters(string);
		if (node instanceof ICompositeNode) {
			List<INode> children = Lists.newArrayList(((ICompositeNode)node).getChildren());
			for (int i=0; i< children.size(); i++) {
				INode n = children.get(i);
				if (n instanceof ILeafNode && "%".equals(((ILeafNode)n).getText())) {
					checkHexValue(value, children, i);
				}
			}
		}

		return value;
	}

	private void checkHexValue (String value, List<INode> children, int indexOfPercent) {
		ValueConverterException ex = new ValueConverterException("The value '" + value + "' is an invalid " + getRuleName(), null, null);
		Object obj1 = (children.size()>indexOfPercent+1 ? children.get(indexOfPercent+1) : null);
		ILeafNode node1AfterPercent = null;
		if (obj1 instanceof ILeafNode) {
			node1AfterPercent = (ILeafNode)obj1;
		}
		Object obj2 = (children.size()>indexOfPercent+2 ? children.get(indexOfPercent+2) : null);
		ILeafNode node2AfterPercent = null;
		if (obj2 instanceof ILeafNode) {
			node2AfterPercent = (ILeafNode)obj2;
		}

		if (node1AfterPercent==null)
			throw ex;

		String valueToTest = null;
		if (node1AfterPercent != null && node2AfterPercent != null) {
			valueToTest = node1AfterPercent.getText() + (node2AfterPercent!=null ? node2AfterPercent.getText() : "");
		}

		if (valueToTest != null || !valueToTest.matches("(\\d|[a-fA-F]){2}.*")) {
			throw ex;
		}
	}
}
