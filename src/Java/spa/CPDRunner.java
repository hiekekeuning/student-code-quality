package spa;
import java.io.*;
import java.sql.SQLException;
import java.util.*;

import net.sourceforge.pmd.cpd.*;

public class CPDRunner
{
	/* Custom CPD runner, runs CPD on all files in a folder separately, without overhead of restarting */
	public static void main(String[] args) throws IOException, SQLException, ClassNotFoundException
	{
	    if (args.length != 4)
	    {
	        System.err.println("Needs 4 args");
		    return;   
	    }
	    
		String dir = args[0];
		int minTokens = Integer.parseInt(args[1]);
		String outFolder = args[2];
		String outFilename = args[3];
		
		final long startTime = System.currentTimeMillis();
		
		// make a list of all files 
		File folder = new File(dir);
		File[] listOfFiles = folder.listFiles();
		List<File> files = new ArrayList<File>(Arrays.asList(listOfFiles));
		System.out.println(String.format("%d files in dir %s", files.size(), dir));

		// configure CPD
		CPDConfiguration config = new CPDConfiguration();
		config.setLanguage(LanguageFactory.createLanguage("Java"));
		config.setMinimumTileSize(minTokens);
		config.setEncoding("UTF-8");
		config.setRenderer(new CSVRenderer());
		/* Has no effect
		config.setIgnoreAnnotations(true);
		config.setIgnoreIdentifiers(true);
		config.setIgnoreLiterals(true);*/
		
		StringBuilder dupOut = new StringBuilder();
		StringBuilder errOut = new StringBuilder();
		int totalCnt = 0;
		ArrayList<Match> matchList = new ArrayList<>();
		
		/*SpaDB db = new SpaDB(SpaDB.testDbUrl);
		PreparedStatement ps = db.startAddIssues();*/
		for(File file: files)
		{
			try
			{
				matchList.clear();
				
				// run CPD on this file
				CPD cpd = new CPD(config);
				cpd.add(file);
				cpd.go();
				
				// collect results
				Iterator<Match> it = cpd.getMatches();
				while(it.hasNext())
				{
					Match m = it.next();
					matchList.add(m);
				}
				if (!matchList.isEmpty()) // write to output
				{
					String csv = config.getRenderer().render(cpd.getMatches());
					dupOut.append(csv.substring(csv.indexOf('\n') + 1)); // remove header
					totalCnt += matchList.size();
					
					// db
					/*String filename = file.getName();
					String filenameNoEx = filename.substring(0, filename.lastIndexOf("."));
					Snapshot ss = Utils.getSSFromFilename(filenameNoEx);
				
					db.addIssue(ps, ss.sfid,ss.evid, "Duplicate", cnt);
					*/
				}
			}
			catch (Exception e)
			{
				errOut.append(file.getName() + "," + e.getMessage() + "\n");
			}
		}

		/*db.commitStat(ps);*/
		
		// write output to files
		try
		{
			String name = folder.getName();
			new FileReporter(new File(outFolder,outFilename)).report(dupOut.toString()); //name + "-cpd" + minTokens + ".csv"
			new FileReporter(new File(outFolder,name + "-cpd" + minTokens + "-err.csv")).report(errOut.toString());
		} catch (ReportException e)
		{
			e.printStackTrace();
		}
		System.out.println("Total nr of dups found " + totalCnt);
		
		final long endTime = System.currentTimeMillis();
		System.out.println("Total execution time in s: " + (endTime - startTime)/1000 );
	}

}
