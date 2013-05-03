package util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ExceptionHandler implements Thread.UncaughtExceptionHandler {
    private static final Logger log = LoggerFactory.getLogger(ExceptionHandler.class);
    
    public void handle( Throwable t ) {
        log.error( "Uncaught exception on the EDT: ", t );
    }
    
    @Override
    public void uncaughtException( Thread t, Throwable e ) {
        log.error( "Uncaught exception on {}: ", t, e );
    }
    
    public void installExceptionHandler() {
        Thread.setDefaultUncaughtExceptionHandler( this );
        System.setProperty("sun.awt.exception.handler",
                this.getClass().getName());
    }
}
