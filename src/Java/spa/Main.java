package spa;

import java.io.*;
import java.sql.*;

public class Main
{
    public static final String testDbUrl = "jdbc:sqlite:C:\\replace_by_dir\\spa.db";
	public static final String inDir4days = "C:\\replace_by_dir\\4days", outDir4days = "C:\\replace_by_dir\\4days";
    public static final String outFileNameFix = "C:\\replace_by_dir\\issueFixing.csv";
    
	public static void main(String[] args) throws IOException, SQLException, Exception
	{
	    
	    /* Issue selection step 2 */
		//extract4Days(inDir4days, outDir4days);
	    
	    BlackboxDB blackboxDB = new BlackboxDB(testDbUrl);
	    
	    /* Extension selection */ 
        //blackboxDB.showAllExtensionsSelected4Weeks();
        //blackboxDB.showNrOfStartupEventsFourWeeks();
        
	    /* Storing blackbox data */
		//fillSpaDB(blackboxDB);
		
		/* Find out min and max event ids for weeks */
		/*blackboxDB.getIdInfo(BlackboxDB.dateLimitWeek1);
		blackboxDB.getIdInfo(BlackboxDB.dateLimitWeek2);
		blackboxDB.getIdInfo(BlackboxDB.dateLimitWeek3);
		blackboxDB.getIdInfo(BlackboxDB.dateLimitWeek4);*/
		
		/*blackboxDB.showEventInfo(BlackboxDB.dateLimitWeek1);
		blackboxDB.showEventInfo(BlackboxDB.dateLimitWeek2);
		blackboxDB.showEventInfo(BlackboxDB.dateLimitWeek3);
		blackboxDB.showEventInfo(BlackboxDB.dateLimitWeek4);*/
	
        /* Reporting - RQ2 */
		//issueFixing(outFileNameFix);
	}
	
	public static void extract4Days(final String inFolder, final String outFolder) throws IOException
	{
		// 4 days for issue selection
		ExtractCode.extractSourceCodeFile(2014, 9, 8, inFolder, outFolder); 
		ExtractCode.extractSourceCodeFile(2014, 12, 8, inFolder, outFolder);
		ExtractCode.extractSourceCodeFile(2015, 3, 9, inFolder, outFolder);
		ExtractCode.extractSourceCodeFile(2015, 6, 8, inFolder, outFolder);
	}
    
	public static void fillSpaDB(BlackboxDB blackboxDB) throws ClassNotFoundException, SQLException
	{
				
		/* Filling the local spa-database*/
	    final int[] idsSep = {332591825, 342090651};
	    final int[] idsDec = {497097058, 509144743};
	    final int[] idsMar = {609983633, 620450771};
	    final int[] idsJun = {729901911, 735813476};
	
		blackboxDB.getSuccessfulCompileEvents(idsSep[0],idsSep[1]); //sep, Nr of records: 576629 Execution time	 104 sec
		blackboxDB.getSuccessfulCompileEvents(idsDec[0],idsDec[1]); //dec, Nr of records: 729805 Execution time	 111 sec
		blackboxDB.getSuccessfulCompileEvents(idsMar[0],idsMar[1]); //mrt, Nr of records: 637564 Execution time	 70 sec
		blackboxDB.getSuccessfulCompileEvents(idsJun[0],idsJun[1]); //jun, Nr of records: 366567 Execution time	 41 sec
		
		blackboxDB.getSnapshots(idsSep[0],idsSep[1]); // Nr of records: 633077 Execution time	 19 sec
		blackboxDB.getSnapshots(idsDec[0],idsDec[1]); // Nr of records: 885914		Execution time	 38 sec
		blackboxDB.getSnapshots(idsMar[0],idsMar[1]); // Nr of records: 818682		Execution time	 36 sec
		blackboxDB.getSnapshots(idsJun[0],idsJun[1]); // Nr of records: 491205		Execution time	 18 sec
				
		/* Extensions */
		
		blackboxDB.getStartupEvents(idsSep[0],idsSep[1]); //Nr of records: 147967 Execution time	 19 sec
		blackboxDB.getStartupEvents(idsDec[0],idsDec[1]); //Nr of records: 161879 Execution time	 114 sec
		blackboxDB.getStartupEvents(idsMar[0],idsMar[1]); //Nr of records: 143637 Execution time	 130 sec
		blackboxDB.getStartupEvents(idsJun[0],idsJun[1]); //Nr of records: 84788 Execution time	 76 sec
		
		blackboxDB.getExtensions(idsSep[0],idsSep[1]); //Nr of records: 3470 Execution time	 19 sec
		blackboxDB.getExtensions(idsDec[0],idsDec[1]); //Nr of records: 5054 Execution time	 20 sec
		blackboxDB.getExtensions(idsMar[0],idsMar[1]); //Nr of records: 3914 Execution time	 15 sec
		blackboxDB.getExtensions(idsJun[0],idsJun[1]); // Nr of records: 1688 Execution time	 17 sec
	}
	
	public static void issueFixing(final String outFileNameFix) throws IOException, SQLException, Exception
	{
		// create file
		new SpaDB(testDbUrl).issueFixing(outFileNameFix); 
		// process file
		new Fixing().issueFixing(outFileNameFix);
	}
}





