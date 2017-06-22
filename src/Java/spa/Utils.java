package spa;

import java.io.PrintWriter;
import java.nio.file.Paths;
import java.util.*;

class Utils 
{
	public static void codeToFile(String targetFolder, String name, String code)
	{
		try
		{
			PrintWriter writer = new PrintWriter(targetFolder + name + ".java", "UTF-8");
			writer.print(code);
			writer.close();
		}
		catch(Exception e)
		{
			System.err.println("No file created");
		}
	}
	
	/*
	 * f10000213-e370394352 -> (10000213, 370394352)
	 */
	public static Snapshot getSSFromFilename(String filename)
	{
		int dashIndex = filename.indexOf('-');
		long sfid = Long.parseLong(filename.substring(1, dashIndex));
		long evid = Long.parseLong(filename.substring(dashIndex + 2));
		return new Snapshot(evid, sfid);
	}
	
	/*
	 * f10000213-e370394352.java -> 10000213
	 */
	public static String getSFID(String fileName)
	{
		String fn = Paths.get(fileName).getFileName().toString(); //remove path
		int idx = fn.indexOf("-"); //find first -
		return fn.substring(1, idx); //remove "f" + rest
	}
	
	/*
	 * "text" -> text
	 */
	public static String remQuotes(String s)
	{
		return s.substring(1, s.length() - 1); 
	}
	
	/* 
	 * Sort map by value
	 */
	public static <K, V extends Comparable<? super V>> Map<K, V> sortByValue( Map<K, V> map )
    {
        List<Map.Entry<K, V>> list =
            new LinkedList<>( map.entrySet() );
        Collections.sort( list, new Comparator<Map.Entry<K, V>>()
        {
            @Override
            public int compare( Map.Entry<K, V> o1, Map.Entry<K, V> o2 )
            {
                return ( o2.getValue() ).compareTo( o1.getValue() );
            }
        } );
    
        Map<K, V> result = new LinkedHashMap<>();
        for (Map.Entry<K, V> entry : list)
        {
            result.put( entry.getKey(), entry.getValue() );
        }
        return result;
    }
	
    public static <K, V> void printMap(Map<K, V> map)
    {
        for (K key : map.keySet())
        {
            V val = map.get(key);
            System.out.println(key + ": " + val);
        }
    }
}

class Snapshot
{
	public long evid;
	public long sfid;
	
	public Snapshot(long evid, long sfid)
	{
		this.evid = evid;
		this.sfid = sfid;
	}
}