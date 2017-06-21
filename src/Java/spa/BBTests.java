package spa;

import static org.junit.Assert.*;
import org.junit.Test;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class BBTests
{
    @Test
    public void filenameTest()
    {
        Snapshot s = Utils.getSSFromFilename("f10000213-e370394352");
        assertEquals(370394352, s.evid);
        assertEquals(10000213, s.sfid);

        String sfid = Utils.getSFID("f12345-e7890.java");
        assertEquals("12345", sfid);
    }

    @Test
    public void quoteTest()
    {
        assertEquals("hallo", Utils.remQuotes("\"hallo\""));
    }

    @Test
    public void solvedTest()
    {
        Integer [] test1 = new Integer [] {2,1,3,0,0,4,2};
        List<Integer> l = new ArrayList<>(Arrays.asList(test1));
        
        assertEquals(6, Fixing.solved(l));
        assertEquals(8, Fixing.appeared(l));
    }
    
}
