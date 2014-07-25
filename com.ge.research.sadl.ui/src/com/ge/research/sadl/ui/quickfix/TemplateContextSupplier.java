package com.ge.research.sadl.ui.quickfix;

import java.util.Collections;
import java.util.Map;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;

/**
 * This class provides properties for a template's context. It is intended to be
 * sub-classed within a QuickfixProvider.
 */
public class TemplateContextSupplier {

	/**
	 * @return Returns a variableName2variableValue-mapping. This are the
	 *         initial values for variables when the template is applied.
	 */
	public Map<String, String> getVariables(IDocument doc,
			IQuickAssistInvocationContext cactx) throws Exception {
		return Collections.emptyMap();
	}

	/**
	 * @return The position in the document at which the template is applied.
	 */
	public Position getPosition(IDocument doc,
			IQuickAssistInvocationContext cactx) throws Exception {
		int statementEnd = doc.get().indexOf(".\n", cactx.getOffset());
		return new Position(statementEnd + 2);
	}

	/**
	 * @return A string that is placed before the template when the template is
	 *         applied.
	 */
	public String getPrefix(IDocument doc, IQuickAssistInvocationContext cactx)
			throws Exception {
		return "";
	}

	/**
	 * @return A string that is placed after the template when the template is
	 *         applied.
	 */
	public String getPostfix(IDocument doc, IQuickAssistInvocationContext cactx)
			throws Exception {
		return "\n";
	}

}
