/************************************************************************
 * Copyright � 2007-2010 - General Electric Company, All Rights Reserved
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
 * $Revision: 1.2 $ Last modified on   $Date: 2015/01/09 22:01:04 $
 ***********************************************************************/

package com.ge.research.sadl.ui;

import java.io.IOException;
import java.util.HashMap;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.graphics.Color;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IConsoleView;
import org.eclipse.ui.console.IOConsoleOutputStream;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.xtext.util.IAcceptor;

import com.ge.research.sadl.builder.MessageManager;
import com.ge.research.sadl.builder.MessageManager.HyperlinkInfo;
import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.builder.MessageManager.SadlMessage;
import com.ge.research.sadl.utils.SadlConsole;

public final class SadlEclipseConsole implements IAcceptor, SadlConsole {
	private static final String SADL = "SADL";

	private static SadlEclipseConsole mInstance = null;
	
	private static MessageConsole console = null;
	private static IOConsoleOutputStream stdStream = null;
	private static IOConsoleOutputStream errStream = null;
	private static boolean stdStreamFlushed = true;
	private static boolean errStreamFlushed = true;

	private SadlEclipseConsole () { }

	/**
	 * Singleton pattern to match Eclipse's console plugin's behavior.
	 * @return
	 */
	public static synchronized SadlEclipseConsole getInstance() {
		if (mInstance == null)
			mInstance = new SadlEclipseConsole ();

		return mInstance;
	}

	/**
	 * Look for an existing/open console and return it, 
	 * or if it does not exist, create a new one and return it.
	 *
	 * @return A non null message console
	 */
	static MessageConsole findOrCreateConsole() {
		if (console != null) {
			return console;
		}
		final ConsolePlugin plugin = ConsolePlugin.getDefault();
		final IConsoleManager conMan = plugin.getConsoleManager();
		final IConsole[] existing = conMan.getConsoles();

		for (final IConsole element : existing) {
			if (SADL.compareTo(element.getName()) == 0) {
				console = (MessageConsole)element;
				return (MessageConsole) element;
			}
		}
		
		// failed to find existing console, create one:
		final MessageConsole myConsole = new MessageConsole(SADL, null);
		myConsole.activate();
		conMan.addConsoles(new IConsole[] { myConsole });
		console = myConsole;
		return myConsole;
	}
	
	/**
	 * Method to write a message to the console. Note: caller should put in
	 * newlines as desired in the text to display.
	 * 
	 * @param mtype -- one of the values defined in the enum MessageType
	 * @param lineToConsole -- text to display
	 */
	public static void writeToConsole(MessageType mtype, String lineToConsole) { 
		final MessageType type = mtype;
		final String lineToWrite = lineToConsole; 
//		Display.getDefault().asyncExec(new Runnable() {
//			public void run() { 
				try { 
					getOutputStream(type).write(lineToWrite);
				} 
				catch (Exception ex) { 
						///if you want to - do something 
					ex.printStackTrace();
				}
//			}
//		}
//		);
	}

	/**
	 * Method to write a message to the console with a hyperlink to a file. 
	 * Note: caller should put in newlines as desired in the text to display.
	 * 
	 * @param mtype -- one of the values defined in the enum MessageType
	 * @param lineToConsole -- text to display
	 * @param file -- the file to which the hyperlink links
	 * @param fileOffset -- offset in the 
	 * @param fileLength
	 */
	public static void writeToConsoleWithHyperlink(MessageType mtype, String lineToConsole, int consoleOffset, int consoleLength, 
			IFile file, int fileOffset, int fileLength, int fileLineNo) { 
		final MessageType type = mtype;
		final String lineToWrite = lineToConsole; 
		final IFile f = file;
		final int foffset = fileOffset;
		final int flength = fileLength;
		final int flineno = fileLineNo;
		final int oset = consoleOffset;
		final int len = consoleLength;		
		try { 
			MessageConsole mc = findOrCreateConsole();
			int initialLength = mc.getDocument().getLength();
			getOutputStream(type).write(lineToWrite);
			getOutputStream(type).flush();
////			int finalLength = mc.getDocument().getLength();
//			if (foffset >= 0 && flength >= 0) {
//				try {
//					FileLink fileLink = new FileLink(f, null, foffset, flength, flineno);
//					int ilen = len;
//					if (len < 0) {
//						ilen = lineToWrite.length();
//					}
//					int ioset = initialLength + oset;
//					if (ioset < 0) {
//						ioset = initialLength;
//					}
////					if (ioset > finalLength - ilen) {
////						ioset = finalLength - ilen;
////					}
//					mc.addHyperlink(fileLink, ioset, ilen);
//				}
//				catch (Exception e) {
////					e.printStackTrace();
//				}
//			}
		} 
		catch (Exception ex) { 
				///if you want to - do something 
			ex.printStackTrace();
		}
	}

	/**
	 * Return a new output stream of the given type. If the console does
	 * not exist it will be created. 
	 * 
	 * @param type -- message type, one of the enum MessageType
	 * @return An open stream for writing to.
	 */
	public static IOConsoleOutputStream getOutputStream(final MessageType type) {
		MessageConsole ib = findOrCreateConsole();
		if (type.equals(MessageType.INFO)) {
			if (stdStream == null) {
				stdStream = ib.newOutputStream();
				stdStream.setActivateOnWrite(true);
			}
			if (errStream != null && !errStreamFlushed) {
				try {
					errStream.flush();
					errStreamFlushed = true;
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			stdStreamFlushed = false;	// assume that this is being written to
			return stdStream;
		}
		else if (type.equals(MessageType.ERROR)) {
			if (errStream == null) {
				errStream = ib.newOutputStream();
				errStream.setActivateOnWrite(true);
				errStream.setColor(new Color(null, 255, 0, 0));
			}
			if (stdStream != null && !stdStreamFlushed) {
				try {
					stdStream.flush();
					stdStreamFlushed = true;
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
			}
			errStreamFlushed = false;	// assume that this is being written to
			return errStream;
		}
		else {
			return getOutputStream(MessageType.INFO);
		}
	}

	/**
	 * Alternate way to bring up the console view. Don't know
	 * which one is better / the differences.
	 * @param myConsole SadlConsole to show, must not be null.
	 */
	public static void showConsole (IConsole myConsole) {
		IWorkbench workbench = PlatformUI.getWorkbench();
		IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();

		IWorkbenchPage page = window.getActivePage();
		String id = IConsoleConstants.ID_CONSOLE_VIEW;
		IConsoleView view;
		try {
			view = (IConsoleView) page.showView(id);
			view.display(myConsole);
		} catch (PartInitException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Show the application console. If the given console is null, attempt
	 * to find an existing console and use it. If the given console is null
	 * and no existing consoles exist, exit without doing anything.
	 * 
	 * @param myConsole An existing console.
	 */
	public static void displayConsole (IConsole myConsole) {

		// try to grab any old console and display it if given null
		if (myConsole == null) {
			final ConsolePlugin plugin = ConsolePlugin.getDefault();
			final IConsoleManager conMan = plugin.getConsoleManager();
			final IConsole[] existing = conMan.getConsoles();

			if (existing.length == 0)
				return;

			for (final IConsole element : existing)
				myConsole = element;
		}

		ConsolePlugin.getDefault().getConsoleManager().addConsoles(new IConsole[] {myConsole});
		ConsolePlugin.getDefault().getConsoleManager().showConsoleView(myConsole);
	}

	/**
	 * Show the console view with the given ConsoleType. Will not
	 * create one if one does not already exist.
	 * 
	 * @param type Non-null enum of existing console (stdout/err probably safe)
	 */
	public static void displayConsole (final String type) {

		final ConsolePlugin plugin = ConsolePlugin.getDefault();
		final IConsoleManager conMan = plugin.getConsoleManager();
		final IConsole[] existing = conMan.getConsoles();

		for (final IConsole element : existing)
			if (type.toString().compareTo(element.getName()) == 0) {
				ConsolePlugin.getDefault().getConsoleManager().addConsoles(new IConsole[] {element});
				ConsolePlugin.getDefault().getConsoleManager().showConsoleView(element);
				return;
			}
	}

	public static void displayMessages(MessageManager manager) {
		if (manager == null) {
			return;
		}
		MessageConsole mc = findOrCreateConsole();
		IDocument doc = mc.getDocument();
		int startingOffset = doc.getLength();
		HashMap<String, HyperlinkInfo> links = new HashMap<String, HyperlinkInfo>();
		SadlEclipseConsoleListener scl = new SadlEclipseConsoleListener(links, startingOffset);
		doc.addDocumentListener(scl);
		SadlMessage msg;
		while ((msg = manager.getNextMessage()) != null) {
			links = displayMessage(msg, links);
		}
	}

	private static HashMap<String, HyperlinkInfo> displayMessage(SadlMessage msg, HashMap<String, HyperlinkInfo> links) {
		HyperlinkInfo info = msg.getLinkInfo();
		if (info != null && links != null) {
			links.put(msg.getMessage(), msg.getLinkInfo());
		}
		String txt = msg.getMessage();
		if (!txt.endsWith(System.getProperty("line.separator"))) {
			txt += System.getProperty("line.separator");
		}
		writeToConsole(msg.getType(), txt);
		return links;
	}

	@Override
	public void accept(Object t) {
		if (t instanceof SadlMessage) {
			displayMessage((SadlMessage)t, null);
		}
		else if (t instanceof String) {
			writeToConsole(MessageType.INFO, (String)t);
		}
	}

	@Override
	public void print(MessageType type, String message) {
		writeToConsole(type, message);
	}

}
