package com.ge.research.sadl

import com.google.inject.Inject
import org.eclipse.xtext.conversion.ValueConverter
import org.eclipse.xtext.conversion.impl.AbstractDeclarativeValueConverterService
import org.eclipse.xtext.conversion.impl.AbstractIDValueConverter
import org.eclipse.xtext.conversion.impl.INTValueConverter
import org.eclipse.xtext.conversion.impl.STRINGValueConverter
import org.eclipse.xtext.conversion.impl.AbstractValueConverter
import org.eclipse.xtext.conversion.ValueConverterException
import org.eclipse.xtext.nodemodel.INode

class ValueConverterService extends AbstractDeclarativeValueConverterService {
	
	@Inject AbstractIDValueConverter idValueConverter

	@ValueConverter(rule = "ID") def ID() {
		idValueConverter
	}
	
	@ValueConverter(rule = "NAME") def NAME() {
		new AbstractValueConverter<String>() {
			
			override toString(String value) throws ValueConverterException {
				idValueConverter.toString(value)
			}
			
			override toValue(String string, INode node) throws ValueConverterException {
				idValueConverter.toValue(string, node)
			}
			
		}
	}

	@Inject INTValueConverter intValueConverter
	
	@ValueConverter(rule = "INT") def INT() {
		intValueConverter
	}
	
	@Inject STRINGValueConverter stringValueConverter
	
	@ValueConverter(rule = "STRING") def STRING() {
		stringValueConverter
	}

}