/************************************************************************
 * Copyright © 2022 - Natural Semantics, LLC. All Rights Reserved.
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
package com.naturalsemanticsllc.sadl.reasoner;

import com.ge.research.sadl.reasoner.TranslationException;

public class TypedBuiltinFunctionException  extends TranslationException {

    // Needed to avoid a warning.
    private static final long serialVersionUID = 1L;

    public TypedBuiltinFunctionException(String msg) {
        super(msg);
    }
    
    public TypedBuiltinFunctionException(String msg, Exception e) {
    	super(msg, e);
    }

}
