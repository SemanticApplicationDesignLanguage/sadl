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
package com.ge.research.sadl

import com.google.inject.Inject
import org.eclipse.xtend.lib.annotations.Accessors
import org.eclipse.xtext.AbstractRule
import org.eclipse.xtext.conversion.ValueConverter
import org.eclipse.xtext.conversion.impl.AbstractDeclarativeValueConverterService
import org.eclipse.xtext.conversion.impl.IDValueConverter
import org.eclipse.xtext.conversion.impl.INTValueConverter
import org.eclipse.xtext.conversion.impl.STRINGValueConverter

class ValueConverterService extends AbstractDeclarativeValueConverterService {
	
	@Inject NameConverter idValueConverter

	@ValueConverter(rule = "ID") def ID() {
		idValueConverter
	}
	
	@ValueConverter(rule = "DNAME") def DNAME() {
		idValueConverter
	}
	
	@ValueConverter(rule = "QNAME") def QNAME() {
		idValueConverter
	}

	@Inject INTValueConverter intValueConverter
	
	@ValueConverter(rule = "INT") def INT() {
		intValueConverter
	}
	
	@Inject STRINGValueConverter stringValueConverter
	
	@ValueConverter(rule = "STRING") def STRING() {
		stringValueConverter
	}


	static class NameConverter extends IDValueConverter {
		
		@Accessors AbstractRule rule
		
		override protected getLexer() {
			return null
		}
		
	}	
}