using System;
using System.Collections;
using System.Windows.Forms;

namespace VDTimer
{
	public class LVScheduleSort : IComparer
	{
		public int Compare(object x, object y)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_0007: Unknown result type (might be due to invalid IL or missing references)
			//IL_000d: Expected O, but got Unknown
			ListViewItem val = (ListViewItem)x;
			ListViewItem val2 = (ListViewItem)y;
			DateTime t = (DateTime)val.get_SubItems().get_Item(0).get_Tag();
			DateTime t2 = (DateTime)val2.get_SubItems().get_Item(0).get_Tag();
			return DateTime.Compare(t, t2);
		}
	}
}
