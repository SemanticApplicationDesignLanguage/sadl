package com.ge.research.sadl.ui.jena;

import java.io.PrintStream;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.eclipse.ui.console.IOConsoleOutputStream;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.xtext.ui.ecore.EcoreEditorOpener;
import org.eclipse.xtext.ui.ecore.EcoreUiModule;
import org.eclipse.xtext.ui.editor.IURIEditorOpener;
import org.eclipse.xtext.ui.editor.reconciler.XtextReconciler;

import com.ge.research.sadl.builder.MessageManager.MessageType;
import com.ge.research.sadl.ui.SadlConsole;

public class JenaUiModule extends EcoreUiModule {
    public JenaUiModule(AbstractUIPlugin plugin) {
        super(plugin);
		IOConsoleOutputStream iocos = SadlConsole.getOutputStream(MessageType.INFO);
		System.setOut(new PrintStream(iocos));
		System.setErr(new PrintStream(SadlConsole.getOutputStream(MessageType.ERROR)));
		 
//		this is to prevent XtextReconcilerDebuger from continually reporting errors when debug level isn't initialized
		Logger log = Logger.getLogger(XtextReconciler.class);
		log.setLevel(Level.WARN);
    }
    
    public Class<? extends IURIEditorOpener> bindIURIEditorOpener () {
    	return EcoreEditorOpener.class;
    }

}
