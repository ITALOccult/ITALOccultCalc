using System;
using System.Runtime.InteropServices;
using System.Windows.Forms;

namespace winOccult
{
	public static class Program
	{
		[DllImport("mscoree.dll", CharSet = CharSet.Unicode)]
		private static extern bool StrongNameSignatureVerificationEx(string Pth, bool Force, out bool WasVerified);

		[STAThread]
		private static void Main()
		{
			//IL_0023: Unknown result type (might be due to invalid IL or missing references)
			//IL_0044: Unknown result type (might be due to invalid IL or missing references)
			bool WasVerified = false;
			string startupPath = Application.get_StartupPath();
			StrongNameSignatureVerificationEx(startupPath + "\\Occult.exe", Force: true, out WasVerified);
			if (!WasVerified)
			{
				MessageBox.Show("Occult.exe file has been compromised");
			}
			StrongNameSignatureVerificationEx(startupPath + "\\OccultUtilities.dll", Force: true, out WasVerified);
			if (!WasVerified)
			{
				MessageBox.Show("OccultUtilities.dll file has been compromised");
			}
			Application.EnableVisualStyles();
			Application.SetCompatibleTextRenderingDefault(false);
			Application.Run((Form)(object)new frmOccult());
		}
	}
}
