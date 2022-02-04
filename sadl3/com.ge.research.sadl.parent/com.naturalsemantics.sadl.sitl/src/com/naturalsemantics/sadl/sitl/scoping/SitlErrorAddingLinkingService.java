/********************************************************************** 
 * Note: This license has also been called the "New BSD License" or 
 * "Modified BSD License". See also the 2-clause BSD License.
 *
 * Copyright � 2018-2019 - General Electric Company, All Rights Reserved
 * 
 * Projects: ANSWER and KApEESH, developed with the support of the Defense 
 * Advanced Research Projects Agency (DARPA) under Agreement  No.  
 * HR00111990006 and Agreement No. HR00111990007, respectively. 
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, 
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, 
 *    this list of conditions and the following disclaimer in the documentation 
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its 
 *    contributors may be used to endorse or promote products derived 
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF 
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 ***********************************************************************/
package com.naturalsemantics.sadl.sitl.scoping;

import static com.ge.research.sadl.scoping.AmbiguousNameErrorEObjectDescription.AMBIGUOUS_NAME_ALTERNATIVES;

import java.util.Iterator;
import java.util.StringTokenizer;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.resource.IEObjectDescription;

import com.ge.research.sadl.scoping.AmbiguousNameErrorEObjectDescription;
import com.ge.research.sadl.scoping.ErrorAddingLinkingService;

public class SitlErrorAddingLinkingService extends ErrorAddingLinkingService {

	/**
	 * This tests to see if the alternatives are all the same. If they are then it is not
	 * ambiguous as concepts with the same URI are the same concept. This can happen when 
	 * multiple projects are used, each with their own sadlimplicitmodel.
	 * @param eObjectDescription
	 * @return
	 */
	protected boolean alternativesAllSame(IEObjectDescription eObjectDescription) {
		if (eObjectDescription instanceof AmbiguousNameErrorEObjectDescription) {
			boolean ignore = true;
			Iterator<IEObjectDescription> altsitr = ((AmbiguousNameErrorEObjectDescription)eObjectDescription).getAllDescriptions().iterator();
			while (altsitr.hasNext()) {
				IEObjectDescription alt = altsitr.next();
				if (alt instanceof AmbiguousNameErrorEObjectDescription) {
					Iterator<IEObjectDescription> alteobjitr = ((AmbiguousNameErrorEObjectDescription)alt).getEObjDescAlternatives().iterator();
					while (alteobjitr.hasNext()) {
						EObject alteobj = alteobjitr.next().getEObjectOrProxy();
//						if (EcoreUtil2.getContainerOfType(alteobj, AnswerCMStatement.class) == null &&
//								EcoreUtil2.getContainerOfType(alteobj, ModifiedAskStatement.class) == null &&
//								EcoreUtil2.getContainerOfType(alteobj, WhatStatement.class) == null &&
//								EcoreUtil2.getContainerOfType(alteobj, HowManyValuesStatement.class) == null) {
//							ignore = false;
//							break;
//						}
					}
				}
			}
			if (ignore) {
				return true;
			}
		}
		String alternatives = eObjectDescription.getUserData(AMBIGUOUS_NAME_ALTERNATIVES);
		StringTokenizer st = new StringTokenizer(alternatives, ",");
		String lastToken = null;
		boolean allSame = true;
		while (st.hasMoreTokens()) {
			String thisToken = st.nextToken();
			if (lastToken != null && !lastToken.equals(thisToken)) {
				allSame = false;
				break;
			}
			lastToken = thisToken;
		}
		return allSame;
	}

}
