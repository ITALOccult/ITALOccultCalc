using System.Diagnostics;
using System.Windows.Forms;

namespace Occult
{
	public class CDS_Portal
	{
		internal static void CDSPortal(string QueryArgument)
		{
			//IL_001a: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			while (QueryArgument.Contains("  "))
			{
				QueryArgument = QueryArgument.Replace("  ", " ");
			}
			QueryArgument = QueryArgument.Replace("  ", "%20");
			Process.Start("http://cdsportal.u-strasbg.fr/#" + QueryArgument);
		}

		internal static void SimPlay(string QueryArgument)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			while (QueryArgument.Contains("  "))
			{
				QueryArgument = QueryArgument.Replace("  ", " ");
			}
			QueryArgument = QueryArgument.Replace("  ", "%20");
			Process.Start("http://cdsweb.u-strasbg.fr/SimPlay/#target=" + QueryArgument + "&imtype=color");
		}
	}
}
