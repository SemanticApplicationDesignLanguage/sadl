package com.ge.research.sadl.docgen;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.ge.research.sadl.ide.contentassist.antlr.internal.InternalSADLParser;



public class GenerateKeywordSymbolHtmlTable {
	
	public static void main(String [] args) {
		InternalSADLParser isp = new InternalSADLParser(null);
		Object map;
		try {
			map = getTokenMap(isp);
			if (map instanceof Map) {
				List<String> tokenlist = mapToSortedList(map);
				
				for (String s : tokenlist) {
					System.out.println(s);
				}

				StringBuilder sb = new StringBuilder("<html><table>\n");
				int numcol = 3;
				int numrows = tokenlist.size()/3;
				if (tokenlist.size() % numcol != 0) {
					numrows++;
				}
				for (int i = 0; i < numrows; i++) {
					sb.append("<tr>");
					for (int j = 0; j < numcol; j++) {
						sb.append("<td>");
//						sb.append(tokenlist.get(i*numcol+j));
						int idx = j*numrows+i;
						if (idx < tokenlist.size()) {
							sb.append(tokenlist.get(idx));
						}
						sb.append("</td>");
					}
					sb.append("</tr>\n");
				}
				sb.append("</table></html>");
				System.out.println(sb.toString());
			}
		} catch (NoSuchMethodException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (SecurityException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (IllegalAccessException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (IllegalArgumentException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (InvocationTargetException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
	}

	private static List<String> mapToSortedList(Object map) {
		List<String> tokenlist = new ArrayList<String>();
		List<String> symbollist = new ArrayList<String>();
		Iterator<?> itr = ((Map<?, ?>)map).keySet().iterator();
		while (itr.hasNext()) {
			Object val = ((Map<?, ?>)map).get(itr.next());
			String strval = val.toString();
			if (strval.startsWith("'") && strval.length() > 1 && strval.endsWith("'")) {
				strval = strval.substring(1, strval.length() - 1);
			}
			if (strval.startsWith("RULE_")) {
				if (strval.equals("RULE_STRING")) {
					symbollist.add("\"a string\"");
				}
				else if (strval.equals("RULE_SL_COMMENT")) {
					symbollist.add("// a single-line comment");
				}
				else if (strval.equals("RULE_ML_COMMENT")) {
					symbollist.add("/* a multi-line <br> comment */");
				}
			}
			else if (!Character.isLowerCase(strval.charAt(0)) && !Character.isUpperCase(strval.charAt(0))) {
				symbollist.add(strval);
			}
			else {
				tokenlist.add(strval);
			}
		}
		// sort
		Collections.sort(tokenlist);
		Collections.sort(symbollist);
		tokenlist.addAll(symbollist);
		return tokenlist;
	}

	public static Object getTokenMap(InternalSADLParser irp) throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
		Method m = irp.getClass().getMethod("getTokenDefMap");
		Object result = m.invoke(irp);
		return result;
	}

}
