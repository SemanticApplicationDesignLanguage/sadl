/************************************************************************
 * Copyright 2007-2010 - General Electric Company, All Rights Reserved
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
 * $Revision: 1.1 $ Last modified on   $Date: 2013/08/26 18:52:10 $
 ***********************************************************************/
package com.ge.research.sadl.reasoner;

/**
 * This class encapsulates reasoner timing information of a particular type.
 * 
 * @author 200005201
 *
 */
public class ReasonerTiming {
	private String timingCategory;
	private String description;
	private long milliseconds;
	
	public ReasonerTiming() {	
	}
	
	public ReasonerTiming(String categ, String desc, long tms) {
		timingCategory = categ;
		description = desc;
		milliseconds = tms;
	}
	
	public void setTimingCategory(String timingCategory) {
		this.timingCategory = timingCategory;
	}
	
	public String getTimingCategory() {
		return timingCategory;
	}
	
	public void setMilliseconds(long milliseconds) {
		this.milliseconds = milliseconds;
	}
	
	public long getMilliseconds() {
		return milliseconds;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getDescription() {
		return description;
	}
	
	public String toString() {
		return getMilliseconds() + " ms: " + getTimingCategory() + ", " + getDescription();
	}
}
