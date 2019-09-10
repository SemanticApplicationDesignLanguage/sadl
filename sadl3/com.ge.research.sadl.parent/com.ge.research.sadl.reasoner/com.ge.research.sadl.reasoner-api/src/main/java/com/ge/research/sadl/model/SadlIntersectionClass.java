/************************************************************************
 * Copyright \u00a9 2007-2010 - General Electric Company, All Rights Reserved
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

/***********************************************************************
 * $Last revised by: crapo $ 
 * $Revision: 1.2 $ Last modified on   $Date: 2015/07/25 16:27:14 $
 ***********************************************************************/

package com.ge.research.sadl.model;

import java.util.List;

/**
 * This class encapsulates a list of classes, identified by ConceptName
 * instances, by which an unnamed IntersectionClass is identified.
 * 
 * @author 200005201
 *
 */
public class SadlIntersectionClass extends ConceptIdentifier {
    private List<ConceptIdentifier> classes;
    
    public SadlIntersectionClass(List<ConceptIdentifier> classes) {
        this.classes = classes;
    }

    public void setClasses(List<ConceptIdentifier> classes) {
        this.classes = classes;
    }

    public List<ConceptIdentifier> getClasses() {
        return classes;
    }
    
    public String toString() {
    	StringBuilder sb = new StringBuilder('{');
    	for (int i = 0; i < classes.size(); i++) {
    		if (i > 0) {
    			sb.append(" and ");
    		}
    		sb.append(classes.get(i).toString());
    	}
    	sb.append("}");
    	return sb.toString();
    }
}
