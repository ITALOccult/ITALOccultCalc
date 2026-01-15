using System;
using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Mapping
{
	internal class C2A
	{
		private static ProcessStartInfo Info = new ProcessStartInfo();

		internal static void DrawC2AStarChart()
		{
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0371: Invalid comparison between Unknown and I4
			double result = 0.0;
			string text = Utilities.DEGtoDMS(DisplayMPOccultations.MidTime, 2, 0, MinutesOnly: false);
			string text2 = Settings.Default.C2AFieldSize.Replace("°", "");
			if (text2 == "")
			{
				text2 = "1";
			}
			string fileName = Settings.Default.C2A_Path + "\\c2aw.exe";
			string[] array = new string[20]
			{
				"/ow " + string.Format("ra={0,1:f4};de={1,1:f4};", DisplayMPOccultations.StarRA_2000 * (180.0 / Math.PI) / 15.0, DisplayMPOccultations.StarDec_2000 * (180.0 / Math.PI)),
				"eventTime=" + DisplayMPOccultations.UTDate_Numeric.Substring(8, 2) + "/" + DisplayMPOccultations.UTDate_Numeric.Substring(5, 2) + "/" + DisplayMPOccultations.UTDate_Numeric.Substring(0, 4) + " " + text.Substring(0, 2).Trim().PadLeft(2, '0') + ":" + text.Substring(3, 2).Trim().PadLeft(2, '0') + ":" + text.Substring(6, 2).Trim().PadLeft(2, '0') + ";",
				"DST=0;",
				null,
				null,
				null,
				null,
				null,
				null,
				null,
				null,
				null,
				null,
				null,
				null,
				null,
				null,
				null,
				null,
				null
			};
			if (!double.TryParse(Settings.Default.Site_TimeZone_Hrs, out result))
			{
				result = 0.0;
			}
			array[3] = "UTCoffset=" + Convert.ToInt16(result * 60.0) + ";";
			array[4] = "mapType=equatorial;";
			array[5] = "lon=" + Settings.Default.Site_Longitude_dd_d + ";";
			array[6] = "lat=" + Settings.Default.Site_Latitude_dd_d + ";";
			array[7] = "fieldX=" + text2 + ";";
			array[8] = "fieldY=" + text2 + ";";
			array[9] = "singleInstance=" + Convert.ToInt32(Settings.Default.C2APreventMultipleInstances) + ";";
			array[10] = "forcedCatalog=" + Settings.Default.C2ACatalog + ";catalogPolicy=forcedCatalog;";
			array[11] = "starName=" + DisplayMPOccultations.StarNo + ";";
			array[12] = "astName=" + DisplayMPOccultations.AsteroidName + ";";
			array[13] = "astNum=" + DisplayMPOccultations.AsteroidNumber + ";";
			array[14] = "displayAsteroid=1;";
			string text3 = "";
			for (int i = 0; i < 15; i++)
			{
				text3 += array[i];
			}
			if ((Process.GetProcessesByName("c2aw").Length < 1) | !Settings.Default.C2APreventMultipleInstances)
			{
				Process.Start(fileName, text3);
				return;
			}
			byte[] bytes = Encoding.Default.GetBytes(text3);
			using Socket socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
			try
			{
				int port = 5876;
				socket.Connect(new IPEndPoint(IPAddress.Parse("127.0.0.1"), port));
			}
			catch
			{
				if ((int)MessageBox.Show("Unable to connect to the existing instance of C2A.\r\n\r\nYou can try again after closing all existing instance of C2A, or open a new instance\r\n\r\nDo you want to open a new instance?", "C2A from Occult", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) == 6)
				{
					Process.Start(fileName, text3);
				}
				return;
			}
			socket.Send(bytes);
		}
	}
}
