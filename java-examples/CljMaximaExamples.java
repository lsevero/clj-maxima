import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class CljMaximaExamples{
    public static void run()
    {
        // https://clojure.org/reference/java_interop#_calling_clojure_from_java
        IFn require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("clj-maxima.core"));
        IFn mevalp = Clojure.var("clj-maxima.core", "mevalp");
        mevalp.invoke("integrate(x*sin(a*x),x)");
    }
}
