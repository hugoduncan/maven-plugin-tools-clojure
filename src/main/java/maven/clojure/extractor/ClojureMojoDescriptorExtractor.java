package maven.clojure.extractor;

import clojure.lang.RT;
import clojure.lang.Var;

import org.apache.maven.plugin.descriptor.InvalidPluginDescriptorException;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.path.PathTranslator;
import org.apache.maven.tools.plugin.PluginToolsRequest;
import org.apache.maven.tools.plugin.DefaultPluginToolsRequest;
import org.apache.maven.tools.plugin.extractor.MojoDescriptorExtractor;
import org.apache.maven.tools.plugin.extractor.ExtractionException;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Extracts Mojo descriptors from <a href="http://clojure.org">Clojure</a> sources.
 *
 * @version $Id: $
 */
public class ClojureMojoDescriptorExtractor
    implements MojoDescriptorExtractor
{

  /** {@inheritDoc} */
  public List execute( MavenProject project, PluginDescriptor pluginDescriptor )
    throws ExtractionException, InvalidPluginDescriptorException
  {
    return execute( new DefaultPluginToolsRequest( project, pluginDescriptor ) );
  }

  /** {@inheritDoc} */
  public List execute( PluginToolsRequest request )
    throws ExtractionException, InvalidPluginDescriptorException
  {
    MavenProject project = request.getProject();
    ArrayList sources = new ArrayList();
    for ( Iterator i = project.getCompileSourceRoots().iterator(); i.hasNext(); )
    {
      String source = (String) i.next();
      sources.add( source );
      if (source.endsWith( "/java" ) ) {
        sources.add ( source.replace( "/java", "/clojure" ));
      }
    }

    // Get a reference to the foo function.
    try {
      Var require = RT.var("clojure.core", "require");
      Var symbol = RT.var("clojure.core", "symbol");
      require.invoke(
        symbol.invoke(
          "maven.clojure.extractor.clojure-plugin-extractor"));

      Var plugins = RT.var(
        "maven.clojure.extractor.clojure-plugin-extractor",
        "plugin-classes");
      return (List) plugins.invoke(request.getPluginDescriptor(), sources);
    }
    catch (Exception e)
    {
      throw new ExtractionException(
        "Error extracting mojo descriptor from clojure: ",
        e );
    }
  }
}
