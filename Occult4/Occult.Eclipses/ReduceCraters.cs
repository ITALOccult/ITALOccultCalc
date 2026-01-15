using System.IO;
using System.Windows.Forms;

namespace Occult.Eclipses
{
	public class ReduceCraters
	{
		public static CraterReductions CraterReductions;

		public static void ShowCraterReductions()
		{
			//IL_0021: Unknown result type (might be due to invalid IL or missing references)
			if (!File.Exists(Utilities.AppPath + "\\Resource Files\\CraterTimings.bin"))
			{
				MessageBox.Show("This functionality requires a file called CraterTimings.bin\r\nwhich is not present.", "Crater Timings not enabled", (MessageBoxButtons)0);
				return;
			}
			try
			{
				((Control)CraterReductions).Show();
			}
			catch
			{
				CraterReductions = new CraterReductions();
				((Control)CraterReductions).Show();
			}
		}
	}
}
