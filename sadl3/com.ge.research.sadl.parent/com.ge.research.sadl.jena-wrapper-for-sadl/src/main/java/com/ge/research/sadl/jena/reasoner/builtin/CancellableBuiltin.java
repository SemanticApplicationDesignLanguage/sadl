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
package com.ge.research.sadl.jena.reasoner.builtin;

import com.ge.research.sadl.reasoner.IConfigurationManager;
import com.ge.research.sadl.reasoner.InferenceCanceledException;
import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.reasoner.rulesys.RuleContext;
import com.hp.hpl.jena.reasoner.rulesys.builtins.BaseBuiltin;

public abstract class CancellableBuiltin extends BaseBuiltin {

	private IConfigurationManager configMgr;

	protected boolean isCanceled() {
		if (configMgr != null) {
			return configMgr.getInferenceCanceled();
		}
		return false;
	}
	
	protected void checkCanceled(Node[] args, RuleContext context) {
		if (isCanceled()) {
			String argsStr = "";
			for (int i = 0; i < args.length; i++) {
				if (i > 0) argsStr += ", ";
				argsStr += args[i].toString();
			}
			throw new InferenceCanceledException(this, context, "Inference canceled. (Built-in called with args: " + argsStr + ")");
		}
	}

	public void setConfigMgr(IConfigurationManager configMgr) {
		this.configMgr = configMgr;
	}
	
}
