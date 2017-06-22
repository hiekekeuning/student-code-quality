package spa;

import java.io.*;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

public class Fixing
{
    private Map<String, Fix> map = new HashMap<>();
    
    /*
     * RQ2 Issue fixes table
     * Input created in SpaDB.issueFixing
     */
    public void issueFixing(String outFileName) throws IOException
    {
        int nrRecs = 0;
        String prevIssue = "";
        long prevSF = -1;
        ArrayList<Integer> occs = new ArrayList<>();
        
        BufferedReader br = new BufferedReader(new FileReader(outFileName));
        
        // first line
        String line; 
        String[] rec ;
        
        // input file sorted by sf_id, issue, s.event_id
        while ((line = br.readLine()) != null) 
        {
            nrRecs++;
            rec = line.split(",");
            
            // read data 
            long sfid = Long.parseLong(rec[0]); //stores 0 for NULL
            String issue = rec[1];
            Integer count = Integer.parseInt(rec[2]);
            long evid = Long.parseLong(rec[3]); // not needed
            
            if (nrRecs > 1 && (!issue.equals(prevIssue) || sfid != prevSF)) // new issue or source file
            {
                save(prevIssue, occs);
                occs.clear();
            }
            occs.add(count);
            prevIssue = issue;
            prevSF = sfid;
        }
        save(prevIssue, occs);
        br.close();
        
        System.out.println("Records read: " + nrRecs);

        // sort output by fix perc
        List<Fix> c = new ArrayList<>(map.values());
        Collections.sort(c);
        Fix summary = new Fix("Summary");
        System.out.println(StringUtils.rightPad("Issue",40) + StringUtils.rightPad("Appeared",10)
             + StringUtils.rightPad("Fixed",10) + " Perc fixed (%) " + StringUtils.rightPad("End Occc",10));
        for(Fix fix: c)
        {
            double fixPR = Math.round(fix.fixPerc() * 10.0) / 10.0;
            //System.out.println(StringUtils.rightPad(fix.name,40) + StringUtils.rightPad(""+fix.solved,10));
            System.out.println(StringUtils.rightPad(fix.name,40) + "&" + StringUtils.leftPad(fix.appeared + " &", 10)
                + StringUtils.leftPad(fix.solved + " &", 10)
                + StringUtils.leftPad(fixPR + " \\\\", 10)
                );
            
            summary.solved += fix.solved;
            summary.appeared += fix.appeared;
        }
        
        System.out.println();
        System.out.println(StringUtils.rightPad(summary.name,40)
                + StringUtils.rightPad(""+summary.solved,10)
                + StringUtils.rightPad(""+ summary.fixPerc(),10));
    }
    
    // Update hashmap with new findings
    private void save(String issue, List<Integer> occs)
    {
        Fix f = new Fix(issue);
        int total = occs.stream().mapToInt(Integer::intValue).sum();
        if (total > 0)
        {
            f.solved = solved(occs);
            f.appeared = appeared(occs);
        }
        // save in map per issue
        if (issue.isEmpty())
            System.err.println();
        if (map.containsKey(issue))
        {
            Fix old = map.get(issue);
            f.solved   += old.solved;
            f.appeared += old.appeared;
        }
        map.put(issue, f);
    }
    
    public static int solved(List<Integer> occs)
    {
        int solved = 0;
        int [] occA = occs.stream().mapToInt(Integer::intValue).toArray();
        for (int i = 1; i< occA.length; i++)
        {
            int diff = occA[i-1] - occA[i];
            if (diff > 0)
                solved += diff;
        }
        return solved;
    }
    
    public static int appeared(List<Integer> occs)
    {
        int appeared = 0;
        int [] occA = occs.stream().mapToInt(Integer::intValue).toArray();
        int prev = 0;
        for (int i = 0; i < occA.length; i++)
        {
            int diff = occA[i] - prev;
            if (diff > 0)
                appeared += diff;
            prev = occA[i];
        }
        return appeared;
    }
    
}

class Fix implements Comparable<Fix>
{
    public int solved, appeared;
    public String name;
    
    public Fix(String name)
    {
        this.name = name;
        this.solved = 0;
        this.appeared = 0;
    }
    
    @Override
    public int compareTo(Fix o) {
        return new Double(o.fixPerc()).compareTo(new Double(this.fixPerc()));
    }
    
    public double fixPerc()
    {
        return solved * 100.0 / appeared;
    }
    
}
