package spa;

import java.io.*;
import java.util.*;

/*
 * Frequency analysis of a PMD output csv-file
 * ~12s for a 1,4GB input file
 * Used for RQ1 'All Issues'
 * Input file is sorted by file
 */
public class CSVR
{
    public static final String csvFile = "C:\\replace_by_dir\\4daysJ-pmd11rs-final.csv";
    
    public static void main(String[] args) 
    {
	    new CSVR().run(csvFile);
    }
    
    public static final int totalNrUniqueSFs = 90066;
    public Map<String, Integer> setSizes = new HashMap<>();
    
    public CSVR()
    {
        // nr of rules per PMD set
        setSizes.put("Optimization", 12);
        setSizes.put("Design", 57);
        setSizes.put("Controversial", 22);
        setSizes.put("Coupling", 5);
        setSizes.put("Unused Code", 5);
        setSizes.put("Type Resolution", 4);
        setSizes.put("Code Size", 13);
        setSizes.put("Import Statements", 6);
        setSizes.put("Unnecessary", 8);
        setSizes.put("Empty Code", 11);
        setSizes.put("Basic", 24);
    }
    
	public void run(String csvFile)
	{
		long t1 = System.currentTimeMillis();
		
        BufferedReader br = null;
        String line = "";
        String cvsSplitBy = ",";
        
        Set<String> iss = new HashSet<>();
        Set<String> pmdSets = new HashSet<>();
        Map<String, Integer> occs = new HashMap<String, Integer>();
        Map<String, Integer> occsPerSet = new HashMap<String, Integer>();
        List<String> fileList = new LinkedList<>();
        try 
        {
            br = new BufferedReader(new FileReader(csvFile));
            br.readLine(); // skip header
            
            // first line
            line = br.readLine();
            Record rec = new Record(line.split(cvsSplitBy));
           
            String fp = rec.getSFID();
            iss.add(rec.getFullRule());
            fileList.add(fp);
            String prevFp = fp;
            int cnt = 1;
            
            while ((line = br.readLine()) != null) 
            {
            	cnt++;
            	rec = new Record(line.split(cvsSplitBy));
            	fp = rec.getSFID();
            	
            	if (!fp.equals(prevFp)) //new source file
            	{
            		fileList.add(fp);
            		// process rules from prev source file
            		for(String ruleName : iss) //distinct issues that occur
            		{
            		    incrOrAddToMap(occs, ruleName); //found a new occurrence
            		}
            		for(String ruleSet : pmdSets) //distinct sets that occur
                    {
                        incrOrAddToMap(occsPerSet, ruleSet);
                    }
            		
                	iss.clear();
                	pmdSets.clear();
                	prevFp = fp;
            	}
            	iss.add(rec.getFullRule());
            	pmdSets.add(rec.getRuleset());
            }
            
            // Per issue
            occs = Utils.sortByValue(occs);
            Set<String> keys = occs.keySet();
            int totalOccs = 0;
            for (String ruleKey : keys)
    		{
    			int val = occs.get(ruleKey);
    			float percUniqueFiles = (float) val / CSVR.totalNrUniqueSFs * 100;
    			System.out.println(ruleKey + ": " + val + " " + percUniqueFiles);
    			totalOccs += val;
    		}
            System.out.println("---");
            System.out.println("In total: " + totalOccs + " occs of "  + keys.size() + " rules");
            System.out.println("Total nr of issues: " + cnt);
            System.out.println("---\n");
            
            // Rules fired per set
            Map<String, Integer> rulesPerSet = new HashMap<String, Integer>();
            for (String ruleName : keys)
            {
                String s = ruleName.substring(0, ruleName.indexOf("."));
                incrOrAddToMap(rulesPerSet, s);
            }
            for (String setName : rulesPerSet.keySet())
            {
                int val = rulesPerSet.get(setName);
                System.out.println(setName + ": " + val + "/" + setSizes.get(setName));
            }
            
            System.out.println("---");
            
            // By PMD ruleset
            occsPerSet = Utils.sortByValue(occsPerSet);
            for (String ruleKey : occsPerSet.keySet())
            {
                int val = occsPerSet.get(ruleKey);
                System.out.println(ruleKey + ": " + val/ (float) CSVR.totalNrUniqueSFs * 100 + "%");
            }
            
        	// check if same file rules are always adjacent
            System.out.println("---Checks---");
            Set<String> uniqueSFs = new HashSet<>(fileList);
            System.out.println("In total: " + fileList.size() + " source files with issues");
            System.out.println("In total: " + uniqueSFs.size() + " UNIQUE source files with issues");
           
        }
        catch (FileNotFoundException e) 
        {
            e.printStackTrace();
        } 
        catch (IOException e) 
        {
            e.printStackTrace();
        } 
        finally 
        {
            if (br != null) 
            {
                try 
                {
                    br.close();
                } 
                catch (IOException e) 
                {
                    e.printStackTrace();
                }
            }
        }
        long t2 = System.currentTimeMillis();
        System.out.println("Execution time\t " + ( (t2-t1)/1000) + " sec" );  
    }
	
	public static void incrOrAddToMap(Map<String, Integer> occs, String key)
	{
	    if (occs.containsKey(key))
        {
            int oldVal = occs.get(key);
            occs.put(key, oldVal + 1);
        }
        else
        {
            occs.put(key, 1);
        }
	}
	
	public static Map<String, Integer> setToFreq(Set<String> set)
	{
	    Map<String, Integer> map = new HashMap<>();
	    for (String s : set)
        {
            incrOrAddToMap(map, s);
        }
	    return map;
	}
	

}

class Record
{
    private String [] rec;
    public Record(String [] rec)
    {
        this.rec = rec;
        for (int i = 0; i < rec.length; i++)
        {
            rec[i] = Utils.remQuotes(rec[i]);
        }
    }
    public String getRuleset()
    {
        return rec[5];
    }
    public String getRule()
    {
        return rec[6];
    }
    public String getSFID()
    {
        return Utils.getSFID(rec[2]);
    }
    public String getFullRule()
    {
        return getRuleset() + "." + getRule();
    }
}


