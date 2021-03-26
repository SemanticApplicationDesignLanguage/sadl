/************************************************************************
 * Copyright \u00a9 2007, 2008 - General Electric Company, All Rights Reserved
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
 * $Author: crapo $ 
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/26 18:52:13 $
 ***********************************************************************/

package com.ge.research.sadl.reasoner.utils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import jakarta.activation.DataSource;

public class StringDataSource implements DataSource {
	String type = "application/octet-stream";
	String data = null;
	private String name = null;
	
	public StringDataSource(String _data, String _type) {
		data = _data;
		if (_type != null) {
			type = _type;
		}
	}
	
	public String getContentType() {
		return type;
	}

	public InputStream getInputStream() throws IOException {
		ByteArrayInputStream ins = new ByteArrayInputStream(data.getBytes("UTF-8"));
		return ins;
	}

	public String getName() {
		return name;
	}

	public OutputStream getOutputStream() throws IOException {
		return null;
	}

	public void setName(String name) {
		this.name = name;
	}

}
