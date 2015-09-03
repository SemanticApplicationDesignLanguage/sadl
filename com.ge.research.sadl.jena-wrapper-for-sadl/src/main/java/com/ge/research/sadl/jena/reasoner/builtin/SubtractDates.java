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
 * $Revision: 1.1 $ Last modified on   $Date: 2014/01/31 15:41:06 $
 ***********************************************************************/

package com.ge.research.sadl.jena.reasoner.builtin;

import com.hp.hpl.jena.datatypes.xsd.XSDDateTime;
import com.hp.hpl.jena.graph.Node;
import com.hp.hpl.jena.rdf.model.ResourceFactory;
import com.hp.hpl.jena.reasoner.rulesys.BindingEnvironment;
import com.hp.hpl.jena.reasoner.rulesys.RuleContext;
import com.hp.hpl.jena.reasoner.rulesys.Util;
import com.hp.hpl.jena.reasoner.rulesys.builtins.BaseBuiltin;
import java.util.Calendar;

public class SubtractDates extends BaseBuiltin {
	
	private final static int Y = 1;
	private final static int M = 2;
	private final static int D = 3;
	private final static int H = 4;
	private final static int m = 5;
	private final static int S = 6;

    /**
     * Return a name for this builtin, normally this will be the name of the 
     * functor that will be used to invoke it.
     */
    public String getName() {
        return "subtractDates";
    }
    
    /**
     * Return the expected number of arguments for this functor or 0 if the number is flexible.
     */
    public int getArgLength() {
        return 4;
    }

    /**
     * This method is invoked when the builtin is called in a rule body.
     * @param args the array of argument values for the builtin, this is an array 
     * of Nodes, some of which may be Node_RuleVariables.
     * @param length the length of the argument list, may be less than the length of the args array
     * for some rule engines
     * @param context an execution context giving access to other relevant data
     * @return return true if the buildin predicate is deemed to have succeeded in
     * the current environment
     */
    public boolean bodyCall(Node[] args, int length, RuleContext context) {
		checkArgs(length, context);
		BindingEnvironment env = context.getEnv();
		Node n1 = getArg(0, args, context);
		Node n2 = getArg(1, args, context);
		Node n3 = getArg(2, args, context);
		if (n1.isLiteral() && n2.isLiteral()) {
			Object v1 = n1.getLiteralValue();
			Object v2 = n2.getLiteralValue();
			Object v3 = n3.getLiteralValue();
			Node retVal = null;
			if (v1 instanceof XSDDateTime) {
				Calendar cal1 = ((XSDDateTime)v1).asCalendar();
				Calendar cal2 = null;
				double timeDiff = 0;
				if (v2 instanceof XSDDateTime) {
					cal2 = ((XSDDateTime)v2).asCalendar();
				} else if (v2 instanceof Number) {
					timeDiff = ((Number) v2).doubleValue();
				}
				else {
					try {
						throw new Exception("Unhandled argument type in subtractDates: " + v2.getClass().toString());
					}
					catch (Exception e) {
						e.printStackTrace();
					}
					return false;
				}
				int unit = 0;
				if (v3 instanceof String) {
					String units = (String) v3;
					String unitsStart = units.substring(0, 1);
					if (unitsStart.equalsIgnoreCase("y")) {
						unit = Y;
					}
					else if (unitsStart.equalsIgnoreCase("d")) {
						unit = D;
					}
					else if (unitsStart.equalsIgnoreCase("h")) {
						unit = H;
					}
					else if (unitsStart.equalsIgnoreCase("s")) {
						unit = S;
					}
					else if (units.length() > 1){
						unitsStart = units.substring(0, 2);
						if (unitsStart.equalsIgnoreCase("mo")) {		// months
							unit = M;
						}
						else if (unitsStart.equalsIgnoreCase("mi")) {		// minutes
							unit = m;
						}
						else if (unitsStart.equalsIgnoreCase("se")) {		// hours{
							unit = S;
						}
					}
					if (unit == 0) {
						try {
							throw new Exception("Illegal time unit in subtractDates; must be some variation, including unique first letters in any case, of year, month, day, minute, or second.");
						} catch (Exception e) {
							e.printStackTrace();
						}
					}
				}
				else {
					try {
						throw new Exception("Unhandled argument type in subtractDates: " + v3.getClass().toString());
					}
					catch (Exception e) {
						e.printStackTrace();
					}
					return false;
				}
				if (cal2 != null) {
					// difference between two dates, resulting in a number
					switch(unit) {
					case Y:
						timeDiff = getYears(cal2, cal1);
						break;
					case M:
						timeDiff = getMonths(cal2, cal1);
						break;
					case D:
						timeDiff = getDays(cal2, cal1);
						break;
					case H:
						timeDiff = getHours(cal2, cal1);
						break;
					case m:
						timeDiff = getMinutes(cal2, cal1);
						break;
					case S:
						timeDiff = getSeconds(cal2, cal1);
						break;
					}
					retVal = Util.makeDoubleNode(timeDiff);
					return env.bind(args[3], retVal);
				}
				else {
					// this is adding to a date, resulting in another date
					switch(unit) {
					case Y:
						cal1.add(Calendar.YEAR, (int) timeDiff);
						break;
					case M:
						cal1.add(Calendar.MONTH, (int) timeDiff);
						break;
					case D:
						cal1.add(Calendar.DAY_OF_YEAR, (int) timeDiff);
						break;
					case H:
						cal1.add(Calendar.HOUR, (int) timeDiff);
						break;
					case m:
						cal1.add(Calendar.MINUTE, (int) timeDiff);
						break;
					case S:
						cal1.add(Calendar.SECOND, (int) timeDiff);
						break;
					}
					retVal = ResourceFactory.createTypedLiteral(cal1).asNode();
					return env.bind(args[3], retVal);
				}
			}
			else {
				try {
					throw new Exception("Unhandled argument type in subtractDates: " + v1.getClass().toString());
				}
				catch (Exception e) {
					e.printStackTrace();
				}
				return false;
			}
		}
		// Doesn't (yet) handle partially bound cases
		return false;
	}
    
	private static boolean isLeap(Calendar c) {
		return isLeap(c.get(Calendar.YEAR));
	}
	
	private static boolean isLeap(int year) {
		return ((year%400)==0)||((year%100)>0)&&((year%4)==0);
	}
	
	private static final int Feb24DayOfYear = 55; // http://en.wikipedia.org/wiki/February_24 
	private static final int Feb29DayOfYear = 60;
	private static final int DaysInLeapYear = 366;
	private static final int DaysInNormalYear = 365;

	public static synchronized double getYears(Calendar c0,Calendar c1) {
		int doy0 = c0.get(Calendar.DAY_OF_YEAR);
		int days0 = (isLeap(c0)?DaysInLeapYear:DaysInNormalYear) - doy0 + 1;
		int doy1 = c1.get(Calendar.DAY_OF_YEAR);
		int n = DaysInNormalYear;
		if (isLeap(c0) && isLeap(c1)) n = DaysInLeapYear;
		if (isLeap(c0) && (doy0<=Feb29DayOfYear)) n = DaysInLeapYear;
		if (isLeap(c1) && (doy1>=Feb24DayOfYear)) n = DaysInLeapYear;
		double years = (doy1+days0)/(double)n + (c1.get(Calendar.YEAR)-c0.get(Calendar.YEAR)-1);
		return years;
	}

	public static synchronized double getMonths(Calendar c0, Calendar c1) {
		boolean swapped = false;
	    if (c0.after(c1)) {  // swap dates so that d1 is start and d2 is end
	        Calendar swap = c0;
	        c0 = c1;
	        c1 = swap;
	        swapped = true;
	    }
	    double months = (c1.get(java.util.Calendar.MONTH) - c0.get(java.util.Calendar.MONTH)) +
	    					(c1.get(Calendar.DAY_OF_MONTH) - c0.get(Calendar.DAY_OF_MONTH)) / (365 / 12);
	    int y2 = c1.get(java.util.Calendar.YEAR);
	    if (c0.get(java.util.Calendar.YEAR) != y2) {
	        c0 = (java.util.Calendar) c0.clone();
	        do {
	            months += c0.getActualMaximum(java.util.Calendar.MONTH);
	            c0.add(java.util.Calendar.YEAR, 1);
	        } while (c0.get(java.util.Calendar.YEAR) != y2);
	    }
	    if (swapped) {
	    	return -months;
	    }
	    return months;
	}
//	    int doy0 = c0.get(Calendar.DAY_OF_YEAR);
//		int days0 = (isLeap(c0)?DaysInLeapYear:DaysInNormalYear) - doy0 + 1;
//		int doy1 = c1.get(Calendar.DAY_OF_YEAR);
//		int n = DaysInNormalYear;
//		if (isLeap(c0) && isLeap(c1)) n = DaysInLeapYear;
//		if (isLeap(c0) && (doy0<=Feb29DayOfYear)) n = DaysInLeapYear;
//		if (isLeap(c1) && (doy1>=Feb24DayOfYear)) n = DaysInLeapYear;
//		double months = (doy1+days0)/(double)n + (c1.get(Calendar.YEAR)-c0.get(Calendar.YEAR)-1);
//		return months;
//	}
    
	public static synchronized double getDays (Calendar c0, Calendar c1) {
		boolean swapped = false;
	    if (c0.after(c1)) {  // swap dates so that d1 is start and d2 is end
	        Calendar swap = c0;
	        c0 = c1;
	        c1 = swap;
	        swapped = true;
	    }
	    double days = (c1.get(java.util.Calendar.DAY_OF_YEAR) - c0.get(java.util.Calendar.DAY_OF_YEAR)) +
	    				(c1.get(Calendar.HOUR_OF_DAY) - c0.get(Calendar.HOUR_OF_DAY)) / 24;
	    int y2 = c1.get(java.util.Calendar.YEAR);
	    if (c0.get(java.util.Calendar.YEAR) != y2) {
	        c0 = (java.util.Calendar) c0.clone();
	        do {
	            days += c0.getActualMaximum(java.util.Calendar.DAY_OF_YEAR);
	            c0.add(java.util.Calendar.YEAR, 1);
	        } while (c0.get(java.util.Calendar.YEAR) != y2);
	    }
	    if (swapped) {
	    	return -days;
	    }
	    return days;
	}
	
	public static synchronized long getMilliSeconds(Calendar c0, Calendar c1) {
		return c0.getTimeInMillis() - c1.getTimeInMillis();
	}
	
	public static synchronized double getSeconds(Calendar c0, Calendar c1) {
		long diffMS = getMilliSeconds(c0, c1);
		return diffMS / 1000.0;
	}
	
	public static synchronized double getMinutes(Calendar c0, Calendar c1) {
		return getSeconds(c0, c1) / 60.0;
	}
	
	public static synchronized double getHours(Calendar c0, Calendar c1) { 
		return getSeconds(c0, c1) / 3600.0;
	}
	
}
