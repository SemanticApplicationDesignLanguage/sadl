package org.example.domainmodel.ide.lsp.extension;

import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.concurrent.Future;

import org.eclipse.lsp4j.jsonrpc.Launcher;
import org.eclipse.lsp4j.services.LanguageClient;
import org.eclipse.xtext.ide.server.LaunchArgs;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.InputOutput;

import com.google.common.base.Objects;
import com.google.common.io.ByteStreams;
import com.google.inject.Guice;
import com.google.inject.Inject;

public class DomainmodelServerLauncher {
	public static final String LOG = "-log";

	public static final String TRACE = "-trace";

	public static final String NO_VALIDATE = "-noValidate";

	public static void main(String[] args) {
		launch(DomainmodelServerLauncher.class.getName(), args, new DomainmodelServerModule());
	}

	public static void launch(String prefix, String[] args, com.google.inject.Module... modules) {
		LaunchArgs launchArgs = createLaunchArgs(prefix, args);
		DomainmodelServerLauncher launcher = Guice.createInjector(modules).<DomainmodelServerLauncher>getInstance(DomainmodelServerLauncher.class);
		launcher.start(launchArgs);
	}

	@Inject
	private DomainmodelLanguageServer languageServer;

	public void start(LaunchArgs args) {
		try {
			InputOutput.println("Domainmodel Language Server is starting.");
			Launcher<LanguageClient> launcher = Launcher.createLauncher(languageServer,
					LanguageClient.class, args.getIn(), args.getOut(), args.isValidate(), args.getTrace());
			languageServer.connect(launcher.getRemoteProxy());
			Future<Void> future = launcher.startListening();
			InputOutput.println("Domainmodel Language Server has been started.");
			while (!future.isDone()) {
				Thread.sleep(10_000L);
			}
		} catch (InterruptedException e) {
			throw Exceptions.sneakyThrow(e);
		}
	}

	public static LaunchArgs createLaunchArgs(String prefix, String[] args) {
		LaunchArgs launchArgs = new LaunchArgs();
		launchArgs.setIn(System.in);
		launchArgs.setOut(System.out);
		redirectStandardStreams(prefix, args);
		launchArgs.setTrace(getTrace(args));
		launchArgs.setValidate(shouldValidate(args));
		return launchArgs;
	}

	public static PrintWriter getTrace(String[] args) {
		if (shouldTrace(args)) {
			return createTrace();
		}
		return null;
	}

	public static PrintWriter createTrace() {
		return new PrintWriter(System.out);
	}

	public static void redirectStandardStreams(String prefix, String[] args) {
		if (shouldLogStandardStreams(args)) {
			logStandardStreams(prefix);
		} else {
			silentStandardStreams();
		}
	}

	public static boolean shouldValidate(String[] args) {
		return !testArg(args, DomainmodelServerLauncher.NO_VALIDATE);
	}

	public static boolean shouldTrace(String[] args) {
		return testArg(args, DomainmodelServerLauncher.TRACE);
	}

	public static boolean shouldLogStandardStreams(String[] args) {
		return testArg(args, DomainmodelServerLauncher.LOG, "debug");
	}

	public static boolean testArg(String[] args, String... values) {
		for (String arg : args) {
			if (testArg(arg, values)) {
				return true;
			}
		}
		return false;
	}

	public static boolean testArg(String arg, String... values) {
		for(String value : values) {
			if (Objects.equal(value, arg)) {
				return true;
			}
		}
		return false;
	}

	public static void logStandardStreams(String prefix) {
		try {
			FileOutputStream stdFileOut = new FileOutputStream(prefix + "-debug.log");
			redirectStandardStreams(stdFileOut);
		} catch (IOException e) {
			throw Exceptions.sneakyThrow(e);
		}
	}

	public static void silentStandardStreams() {
		redirectStandardStreams(silentOut());
	}

	public static void redirectStandardStreams(OutputStream out) {
		redirectStandardStreams(silentIn(), out);
	}

	public static void redirectStandardStreams(InputStream in, OutputStream out) {
		System.setIn(in);
		System.setOut(new PrintStream(out));
	}

	public static OutputStream silentOut() {
		return ByteStreams.nullOutputStream();
	}

	public static InputStream silentIn() {
		return new ByteArrayInputStream(new byte[0]);
	}
}
