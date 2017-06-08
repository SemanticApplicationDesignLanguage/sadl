/************************************************************************
 * Copyright 2007-2016- General Electric Company, All Rights Reserved
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
package com.ge.research.sadl.utils

import com.ge.research.sadl.sADL.SadlModel
import com.google.common.base.Equivalence
import com.google.common.base.Objects

/**
 * Equivalence for SADL models. Marks two models equal if they have equal
 * base URIs.
 * 
 * @author akos.kitta
 */
class SadlModelEquivalence extends Equivalence<SadlModel> {
	
	/**
	 * The shared (eager) singleton equivalence for SADL models.
	 */
	public static val Equivalence<SadlModel> INSTANCE = new SadlModelEquivalence();
	
	private new() { }
	
	override protected doEquivalent(SadlModel a, SadlModel b) {
		return Objects.equal(a?.baseUri, b?.baseUri);
	}
	
	override protected doHash(SadlModel model) {
		return Objects.hashCode(model?.baseUri);
	}
	
}