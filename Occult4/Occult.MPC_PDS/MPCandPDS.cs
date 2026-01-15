using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Asteroid_Observations;
using Occult.Properties;

namespace Occult.MPC_PDS
{
	public class MPCandPDS : Form
	{
		private HelpNavigator navigator = (HelpNavigator)(-2147483642);

		private IContainer components;

		private Button cmdEditHeaders;

		private Panel panel2;

		private CheckBox chkByYear;

		private CheckBox chkIncludeAll;

		private Button cmdAddMPC;

		private Button cmdMPCreport;

		private Panel panel1;

		private Button button2;

		private Button cmdEditAuthorList;

		private Button cmdMakeShort_CatCorrns;

		private Label label1;

		private Label label2;

		private CheckBox chkTest;

		internal Button cmdNASA_PDS;

		private GroupBox groupBox1;

		private Button cmdDownloadHeaders;

		private RadioButton optLatestYear;

		private TextBox txtAsteroid;

		private CheckBox chkAsteroid;

		private Label label3;

		private TextBox txtEndYear;

		private TextBox txtStartYear;

		private CheckBox chkAsteroids;

		private CheckBox chkPlanets;

		private CheckBox chkExcludeNoPM;

		private Panel panel3;

		private Label label5;

		private Label label4;

		private RadioButton optRAdec;

		private RadioButton optXYZ;

		private Button cmdGetADES;

		private Panel panel4;

		private Button cmdADESfields;

		private ComboBox cmbEndMonth;

		private ComboBox cmbEndYear;

		private CheckBox chkEndDateLimit;

		private Panel panel5;

		private Label label6;

		private TextBox txtADESversion;

		private Label label8;

		private Label label7;

		private ToolTip toolTip1;

		private Button cmdPDS4Viewer;

		private Button cmdPDSweb;

		private CheckBox chkAsteroidSatellites;

		private Label label9;

		private RadioButton optExtraData;

		private CheckBox chkObserverName;

		private TextBox txtObserver;

		private RadioButton optObserverOnly;

		private Label label10;

		private Button cmdMPChelp;

		private Panel panel7;

		private Button cmdMPCsubmit;

		private Button cmdMPCtest;

		private Panel panel8;

		private Label label11;

		private Button cmdClear;

		private Button MPC_Explorer;

		private TextBox txtPDSversion;

		private Label lblPDSversion;

		private Button cmdMPCstatus;

		private Label label13;

		private Panel panel10;

		private Label label12;

		private Panel panel11;

		private Label label14;

		private Label label15;

		private Panel panel12;

		private Label label16;

		private Panel panel13;

		private Label label17;

		private TextBox txtMPC_MEA;

		private TextBox txtMPC_CON;

		private Panel panel14;

		private Label label18;

		private Label label19;

		private Label label100;

		private Label label99;

		private Label label20;

		private Panel panel6;

		private Label label21;

		private Label label22;

		private Button cmdValidateStatus;

		private Button cmdMPC_ServiceDesk;

		private TextBox textBox1;

		public MPCandPDS()
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			InitializeComponent();
		}

		private void MPCandPDS_Load(object sender, EventArgs e)
		{
			((Control)optLatestYear).set_Text("Generate test report for " + DateTime.Now.Year + " only");
			if (!Directory.Exists(MPC.MPC_Path))
			{
				Directory.CreateDirectory(MPC.MPC_Path);
			}
			if (!Directory.Exists(PDS.PDSFilePath))
			{
				Directory.CreateDirectory(PDS.PDSFilePath);
			}
			if (!Directory.Exists(PDS.PDSHeaderFilePath))
			{
				Directory.CreateDirectory(PDS.PDSHeaderFilePath);
			}
			if (!File.Exists(PDS.PDSHeaderFilePath + "HeaderAsteroids.txt"))
			{
				string occultServer = Settings.Default.OccultServer;
				string fileName = "pds_headers.zip";
				string pDSHeaderFilePath = PDS.PDSHeaderFilePath;
				http.DownloadHTTP(occultServer, fileName, pDSHeaderFilePath, unzip: true, gunzip: false, ShowMessages: true);
			}
			if (DateTime.Now.Month > 2)
			{
				TextBox obj = txtStartYear;
				string text;
				((Control)txtEndYear).set_Text(text = DateTime.Now.Year.ToString());
				((Control)obj).set_Text(text);
			}
			else
			{
				TextBox obj2 = txtStartYear;
				string text;
				((Control)txtEndYear).set_Text(text = DateTime.Now.Year.ToString());
				((Control)obj2).set_Text(text);
			}
			cmbEndYear.get_Items().Clear();
			for (int i = 2000; i < 2040; i++)
			{
				cmbEndYear.get_Items().Add((object)i.ToString());
			}
			((ListControl)cmbEndYear).set_SelectedIndex(DateTime.Now.Year - 2000);
			((ListControl)cmbEndMonth).set_SelectedIndex(DateTime.Now.Month - 1);
		}

		internal void cmdEditHeaders_Click(object sender, EventArgs e)
		{
			Asteroid_Observations_Reports.Show_EditPDS_Headers();
		}

		private void cmdNASA_PDS_Click(object sender, EventArgs e)
		{
			PDS.PDS_Version = ((Control)txtPDSversion).get_Text().Trim();
			PDS.CreateNASA_PDS_Pipe_Files(chkTest.get_Checked());
		}

		private void cmdEditAuthorList_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new PDS_AuthorList()).ShowDialog();
		}

		private void cmdAddMPC_Click(object sender, EventArgs e)
		{
			Data_and_Plots.AddMPC_AcceptedData_toHistoricalFile();
		}

		private void cmdMPCreport_Click(object sender, EventArgs e)
		{
			double lastJD = 3000000.0;
			int result = 0;
			int result2 = 1700;
			int result3 = 2200;
			if (chkAsteroid.get_Checked())
			{
				int.TryParse(((Control)txtAsteroid).get_Text(), out result);
			}
			if (chkByYear.get_Checked())
			{
				int.TryParse(((Control)txtStartYear).get_Text(), out result2);
				int.TryParse(((Control)txtEndYear).get_Text(), out result3);
				if (result2 < 1800)
				{
					result2 = 2000;
				}
				if (result3 < result2)
				{
					result3 = result2;
					((Control)txtEndYear).set_Text(result3.ToString());
				}
				if (result3 > DateTime.Now.Year)
				{
					result3 = DateTime.Now.Year;
				}
			}
			if (optLatestYear.get_Checked())
			{
				result2 = (result3 = DateTime.Now.Year);
			}
			if (chkEndDateLimit.get_Checked())
			{
				int year = ((ListControl)cmbEndYear).get_SelectedIndex() + 2000;
				int month = ((ListControl)cmbEndMonth).get_SelectedIndex() + 1;
				int num = Utilities.MaximumDaysInMonth(year, month);
				lastJD = Utilities.JD_from_Date(year, month, num);
			}
			MPC.CreateMPC_Report(Observer: (!chkObserverName.get_Checked()) ? "" : ((Control)txtObserver).get_Text().Trim().ToLower(), Asteroids: chkAsteroids.get_Checked(), Planets: chkPlanets.get_Checked(), AsteroidSatellites: chkAsteroidSatellites.get_Checked(), IncludeAll: chkIncludeAll.get_Checked(), ByYear: chkByYear.get_Checked(), YearStart: result2, YearEnd: result3, LastJD: lastJD, LatestYearOnly: optLatestYear.get_Checked(), AsteroidNumber: result, ObserverOnly: optObserverOnly.get_Checked(), XYZ_Report: optXYZ.get_Checked(), ExcludeStarsWithNoPM: chkExcludeNoPM.get_Checked(), IncludeExtraData: optExtraData.get_Checked());
		}

		private void cmdHelpPDS_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", navigator, (object)"PDS");
		}

		private void cmdMPChelp_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", navigator, (object)"MPCsubmission");
		}

		private void cmdMakeShort_CatCorrns_Click(object sender, EventArgs e)
		{
		}

		private void cmdDownloadHeaders_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("This download will replace the existing Header files with the\r\nversion of those files on the Occult server.\r\n\r\nAny changes you have made to your local Header files will be lost.\r\n\r\n\r\nDo you want to download the Header files?", "Confirm overwrite", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				string occultServer = Settings.Default.OccultServer;
				string fileName = "pds_headers.zip";
				string finalDestination = Utilities.AppPath + "\\Asteroid\\Results\\PDS Files\\Headers\\";
				http.DownloadHTTP(occultServer, fileName, finalDestination, unzip: true, gunzip: false, ShowMessages: true);
			}
		}

		private void optLatestYear_MouseClick(object sender, MouseEventArgs e)
		{
			optLatestYear.set_Checked(!optLatestYear.get_Checked());
		}

		private void txtAsteroid_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()));
		}

		private void txtStartYear_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()));
		}

		private void txtEndYear_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()));
		}

		private void cmdGetADES_Click(object sender, EventArgs e)
		{
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Unknown result type (might be due to invalid IL or missing references)
			string text = Utilities.AppPath + "\\asteroid\\results\\MPC Files";
			string text2 = "ADES_Description.pdf";
			if (!Directory.Exists(text))
			{
				Directory.CreateDirectory(text);
			}
			if (http.DownloadHTTP("https://github.com/IAU-ADES/ADES-Master/raw/master", text2, text + "\\" + text2, unzip: false, gunzip: false, ShowMessages: false))
			{
				MessageBox.Show("The ADES Description .pdf file has been saved in\r\n" + text, "Successful download", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
			else
			{
				MessageBox.Show("The ADES Description .pdf file has not been saved", "Download Failure", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		private void cmdADESfields_Click(object sender, EventArgs e)
		{
			//IL_0050: Unknown result type (might be due to invalid IL or missing references)
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			string text = Utilities.AppPath + "\\asteroid\\results\\MPC Files";
			if (!Directory.Exists(text))
			{
				Directory.CreateDirectory(text);
			}
			string text2 = "ADESFieldValues.html";
			if (http.SaveHTTP_WebPage("https://www.minorplanetcenter.net/iau/info/ADESFieldValues.html", text + "\\" + text2))
			{
				MessageBox.Show("The MPC list of field values has been saved in\r\n" + text, "Successful download", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
			else
			{
				MessageBox.Show("The MPC list of field values has not been saved", "Download Failure", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
		}

		private void cmdPDS4Viewer_Click(object sender, EventArgs e)
		{
			//IL_004f: Unknown result type (might be due to invalid IL or missing references)
			//IL_006a: Unknown result type (might be due to invalid IL or missing references)
			string appPath = Utilities.AppPath;
			string text = "pds4_viewer.exe";
			string text2 = appPath + "/" + text;
			if (!File.Exists(text2))
			{
				http.DownloadHTTP(Settings.Default.OccultServer, "pds4_viewer_windows-1.3.zip", appPath, unzip: true, gunzip: false, ShowMessages: true);
			}
			if (!File.Exists(text2))
			{
				MessageBox.Show("PDS4 Download failed", "Failed download", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			Process.Start(text2);
			MessageBox.Show("When the viewer control opens:\r\n* Click File... Open\r\n* navigate to\r\n   Occult 4/Asteroid/Results/Pds Files\r\nthen to the folder with the relevant YearMonth name.\r\n\r\nFrom within that folder, select the relevant .xml file\r\n\r\nFrom the control that then appears, click\r\n* Table, to view the data\r\n* Label, to view all the column labels. and\r\n* Plot, to create plots from the data.\r\n\r\nOn the Table display, hover the cursor over a column header to view the details of the data in that column.\r\n\r\nThe 'on-line PDS4 viewer guide' button provides a broad indication of the viewer capabilities. There is no Help file for the Viewer.", "PDS4 information", (MessageBoxButtons)0, (MessageBoxIcon)64);
		}

		private void cmdPDSweb_Click(object sender, EventArgs e)
		{
			Process.Start("https://sbnwiki.asteroiddata.org/");
		}

		private void optRAdec_CheckedChanged(object sender, EventArgs e)
		{
			((Control)optExtraData).set_Enabled(optXYZ.get_Checked());
			if (!optXYZ.get_Checked())
			{
				optExtraData.set_Checked(false);
			}
		}

		private void optXYZ_CheckedChanged(object sender, EventArgs e)
		{
			((Control)optExtraData).set_Enabled(optXYZ.get_Checked());
			if (!optXYZ.get_Checked())
			{
				optExtraData.set_Checked(false);
			}
		}

		private void optExtraData_Click(object sender, EventArgs e)
		{
			optExtraData.set_Checked(!optExtraData.get_Checked());
		}

		private void optObserverOnly_Click(object sender, EventArgs e)
		{
			optObserverOnly.set_Checked(!optObserverOnly.get_Checked());
		}

		private void cmdMPCtest_Click(object sender, EventArgs e)
		{
			Process.Start("https://minorplanetcenter.net/submit_psv_test");
		}

		private void cmdValidateStatus_Click(object sender, EventArgs e)
		{
			Process.Start("https://submit-test.minorplanetcenter.net/submission_status");
		}

		private void cmdMPCsubmit_Click(object sender, EventArgs e)
		{
			Process.Start("https://minorplanetcenter.net/submit_psv");
		}

		private void cmdMPCstatus_Click(object sender, EventArgs e)
		{
			Process.Start("https://data.minorplanetcenter.net/wamo/");
		}

		private void cmdClear_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_002d: Invalid comparison between Unknown and I4
			//IL_00e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e6: Invalid comparison between Unknown and I4
			//IL_01a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01aa: Invalid comparison between Unknown and I4
			//IL_02d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02dd: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("This option is to be used when the submission details\r\nof a particular submission need to be removed\r\n\r\nYou will need to specify the submission date.\r\n\r\nDo you want to continue?", "Clear submission details", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			SubmissionDate submissionDate = new SubmissionDate();
			if ((int)((Form)submissionDate).ShowDialog() == 2)
			{
				return;
			}
			string text = "<MPC>" + submissionDate.Year + submissionDate.Month.ToString().PadLeft(2) + submissionDate.Day.ToString().PadLeft(2) + "|";
			_ = "<MPC>" + submissionDate.Year + submissionDate.Month.ToString().PadLeft(2, '_') + submissionDate.Day.ToString().PadLeft(2, '_');
			if ((int)MessageBox.Show("The date to be searched for is\r\n   " + text + "\r\n\r\nIs this correct ", "Confirm date", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			if (Data_and_Plots.Historical_AllEvents.OccEvents.Count < 1000)
			{
				Data_and_Plots.Historical_AllEvents.ReadObservationsFile(HistoricalFile: true, "");
			}
			int num = 0;
			for (int i = 0; i < Data_and_Plots.Historical_AllEvents.OccEvents.Count; i++)
			{
				for (int j = 0; j < Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines.Count; j++)
				{
					if (Data_and_Plots.Historical_AllEvents.OccEvents[i].Lines[j].Contains(text))
					{
						num++;
					}
				}
			}
			if ((int)MessageBox.Show("The number of entries that will be cleared is\r\n   " + num + "\r\n\r\nDo you want to make the replacements?", "Confirm replacement", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			for (int k = 0; k < Data_and_Plots.Historical_AllEvents.OccEvents.Count; k++)
			{
				for (int l = 0; l < Data_and_Plots.Historical_AllEvents.OccEvents[k].Lines.Count; l++)
				{
					if (Data_and_Plots.Historical_AllEvents.OccEvents[k].Lines[l].Contains(text))
					{
						Data_and_Plots.Historical_AllEvents.OccEvents[k].Lines[l] = "        <MPC>||</MPC>";
					}
				}
			}
			if ((int)MessageBox.Show("The MPC report data in memory has been \r\nupdated in memory, and is now ready to be saved.\r\n\r\nDo you want to save the updated set of observations?\r\n\r\nThe existing Asteroid~Observations.xml file will be renamed to\r\n\r\nAsteroid~Observations_Pre_MPCreset_" + DateTime.Now.Year + "-" + Utilities.Months[DateTime.Now.Month].PadLeft(2, '0') + "-" + DateTime.Now.Day.ToString().PadLeft(2, '0') + ".xml", "Confirm write", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
			{
				File.Move(Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile, Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile.Replace(".xml", "_Pre_MPCreset_" + text.Replace("<", "_").Replace(">", "_").Replace("|", "")
					.Replace(" ", "_") + ".xml"));
				Data_and_Plots.Historical_AllEvents.WriteHistoricalObservationsFile(HistoricalFile: true, "");
			}
			Data_and_Plots.Historical_AllEvents.OccEvents.Clear();
		}

		private void MPC_Explorer_Click(object sender, EventArgs e)
		{
			Process.Start("https://data.minorplanetcenter.net/explorer/");
		}

		private void cmdMPC_ServiceDesk_Click(object sender, EventArgs e)
		{
			Process.Start("https://mpc-service.atlassian.net/servicedesk/customer/portals");
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Expected O, but got Unknown
			//IL_007f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Expected O, but got Unknown
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Expected O, but got Unknown
			//IL_00a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00aa: Expected O, but got Unknown
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Expected O, but got Unknown
			//IL_00b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c0: Expected O, but got Unknown
			//IL_00c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Expected O, but got Unknown
			//IL_00cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d6: Expected O, but got Unknown
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Expected O, but got Unknown
			//IL_00e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Expected O, but got Unknown
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0102: Expected O, but got Unknown
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Expected O, but got Unknown
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Expected O, but got Unknown
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Expected O, but got Unknown
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Expected O, but got Unknown
			//IL_012f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Expected O, but got Unknown
			//IL_013a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Expected O, but got Unknown
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Expected O, but got Unknown
			//IL_0150: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Expected O, but got Unknown
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Expected O, but got Unknown
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			//IL_0171: Unknown result type (might be due to invalid IL or missing references)
			//IL_017b: Expected O, but got Unknown
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0186: Expected O, but got Unknown
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Expected O, but got Unknown
			//IL_0192: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Expected O, but got Unknown
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Expected O, but got Unknown
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b2: Expected O, but got Unknown
			//IL_01b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01bd: Expected O, but got Unknown
			//IL_01be: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c8: Expected O, but got Unknown
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d3: Expected O, but got Unknown
			//IL_01d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01de: Expected O, but got Unknown
			//IL_01df: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e9: Expected O, but got Unknown
			//IL_01ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f4: Expected O, but got Unknown
			//IL_01f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ff: Expected O, but got Unknown
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_020b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0215: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_0220: Expected O, but got Unknown
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			//IL_022c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0236: Expected O, but got Unknown
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0241: Expected O, but got Unknown
			//IL_0242: Unknown result type (might be due to invalid IL or missing references)
			//IL_024c: Expected O, but got Unknown
			//IL_024d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0257: Expected O, but got Unknown
			//IL_0258: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Expected O, but got Unknown
			//IL_0263: Unknown result type (might be due to invalid IL or missing references)
			//IL_026d: Expected O, but got Unknown
			//IL_026e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0278: Expected O, but got Unknown
			//IL_0279: Unknown result type (might be due to invalid IL or missing references)
			//IL_0283: Expected O, but got Unknown
			//IL_0284: Unknown result type (might be due to invalid IL or missing references)
			//IL_028e: Expected O, but got Unknown
			//IL_028f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0299: Expected O, but got Unknown
			//IL_029a: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a4: Expected O, but got Unknown
			//IL_02a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_02af: Expected O, but got Unknown
			//IL_02b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ba: Expected O, but got Unknown
			//IL_02bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c5: Expected O, but got Unknown
			//IL_02c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d0: Expected O, but got Unknown
			//IL_02d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02db: Expected O, but got Unknown
			//IL_02dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e6: Expected O, but got Unknown
			//IL_02e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f1: Expected O, but got Unknown
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Expected O, but got Unknown
			//IL_02fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0307: Expected O, but got Unknown
			//IL_0308: Unknown result type (might be due to invalid IL or missing references)
			//IL_0312: Expected O, but got Unknown
			//IL_0313: Unknown result type (might be due to invalid IL or missing references)
			//IL_031d: Expected O, but got Unknown
			//IL_031e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0328: Expected O, but got Unknown
			//IL_0329: Unknown result type (might be due to invalid IL or missing references)
			//IL_0333: Expected O, but got Unknown
			//IL_0334: Unknown result type (might be due to invalid IL or missing references)
			//IL_033e: Expected O, but got Unknown
			//IL_033f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0349: Expected O, but got Unknown
			//IL_034a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0354: Expected O, but got Unknown
			//IL_0355: Unknown result type (might be due to invalid IL or missing references)
			//IL_035f: Expected O, but got Unknown
			//IL_0360: Unknown result type (might be due to invalid IL or missing references)
			//IL_036a: Expected O, but got Unknown
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Expected O, but got Unknown
			//IL_0376: Unknown result type (might be due to invalid IL or missing references)
			//IL_0380: Expected O, but got Unknown
			//IL_0381: Unknown result type (might be due to invalid IL or missing references)
			//IL_038b: Expected O, but got Unknown
			//IL_038c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0396: Expected O, but got Unknown
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a1: Expected O, but got Unknown
			//IL_03a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ac: Expected O, but got Unknown
			//IL_03b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03bd: Expected O, but got Unknown
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c8: Expected O, but got Unknown
			//IL_03c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d3: Expected O, but got Unknown
			//IL_09f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_09fc: Expected O, but got Unknown
			//IL_0ab8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ac2: Expected O, but got Unknown
			//IL_0d18: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d22: Expected O, but got Unknown
			//IL_2084: Unknown result type (might be due to invalid IL or missing references)
			//IL_208e: Expected O, but got Unknown
			//IL_210f: Unknown result type (might be due to invalid IL or missing references)
			//IL_2119: Expected O, but got Unknown
			//IL_2974: Unknown result type (might be due to invalid IL or missing references)
			//IL_297e: Expected O, but got Unknown
			//IL_2e12: Unknown result type (might be due to invalid IL or missing references)
			//IL_2e1c: Expected O, but got Unknown
			//IL_351e: Unknown result type (might be due to invalid IL or missing references)
			//IL_3528: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(MPCandPDS));
			cmdEditHeaders = new Button();
			panel2 = new Panel();
			panel14 = new Panel();
			label20 = new Label();
			label99 = new Label();
			label100 = new Label();
			label19 = new Label();
			label18 = new Label();
			txtMPC_MEA = new TextBox();
			txtMPC_CON = new TextBox();
			panel4 = new Panel();
			label22 = new Label();
			txtADESversion = new TextBox();
			label8 = new Label();
			label7 = new Label();
			cmdADESfields = new Button();
			cmdGetADES = new Button();
			panel8 = new Panel();
			label15 = new Label();
			cmdClear = new Button();
			cmdAddMPC = new Button();
			panel7 = new Panel();
			cmdValidateStatus = new Button();
			label13 = new Label();
			MPC_Explorer = new Button();
			cmdMPCstatus = new Button();
			cmdMPCsubmit = new Button();
			cmdMPCtest = new Button();
			cmdMPChelp = new Button();
			groupBox1 = new GroupBox();
			panel11 = new Panel();
			label14 = new Label();
			chkByYear = new CheckBox();
			optExtraData = new RadioButton();
			panel3 = new Panel();
			label5 = new Label();
			label4 = new Label();
			optRAdec = new RadioButton();
			optXYZ = new RadioButton();
			label3 = new Label();
			txtStartYear = new TextBox();
			txtEndYear = new TextBox();
			panel10 = new Panel();
			label12 = new Label();
			label11 = new Label();
			chkAsteroidSatellites = new CheckBox();
			chkAsteroids = new CheckBox();
			label9 = new Label();
			chkPlanets = new CheckBox();
			panel5 = new Panel();
			optObserverOnly = new RadioButton();
			label10 = new Label();
			txtObserver = new TextBox();
			optLatestYear = new RadioButton();
			chkExcludeNoPM = new CheckBox();
			chkObserverName = new CheckBox();
			label6 = new Label();
			chkIncludeAll = new CheckBox();
			cmbEndMonth = new ComboBox();
			cmbEndYear = new ComboBox();
			chkEndDateLimit = new CheckBox();
			txtAsteroid = new TextBox();
			chkAsteroid = new CheckBox();
			cmdMPCreport = new Button();
			label1 = new Label();
			cmdMakeShort_CatCorrns = new Button();
			panel1 = new Panel();
			panel6 = new Panel();
			label21 = new Label();
			cmdPDSweb = new Button();
			cmdPDS4Viewer = new Button();
			panel13 = new Panel();
			txtPDSversion = new TextBox();
			label17 = new Label();
			lblPDSversion = new Label();
			cmdDownloadHeaders = new Button();
			cmdNASA_PDS = new Button();
			chkTest = new CheckBox();
			panel12 = new Panel();
			cmdEditAuthorList = new Button();
			label16 = new Label();
			label2 = new Label();
			button2 = new Button();
			toolTip1 = new ToolTip(components);
			cmdMPC_ServiceDesk = new Button();
			textBox1 = new TextBox();
			((Control)panel2).SuspendLayout();
			((Control)panel14).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)panel8).SuspendLayout();
			((Control)panel7).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)panel11).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)panel10).SuspendLayout();
			((Control)panel5).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)panel6).SuspendLayout();
			((Control)panel13).SuspendLayout();
			((Control)panel12).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdEditHeaders).set_BackColor(Color.AntiqueWhite);
			((Control)cmdEditHeaders).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdEditHeaders).set_Location(new Point(43, 26));
			((Control)cmdEditHeaders).set_Name("cmdEditHeaders");
			((Control)cmdEditHeaders).set_Size(new Size(117, 34));
			((Control)cmdEditHeaders).set_TabIndex(0);
			((Control)cmdEditHeaders).set_Text("PDS Header files");
			((ButtonBase)cmdEditHeaders).set_UseVisualStyleBackColor(false);
			((Control)cmdEditHeaders).add_Click((EventHandler)cmdEditHeaders_Click);
			((Control)panel2).set_BackColor(Color.Honeydew);
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)panel14);
			((Control)panel2).get_Controls().Add((Control)(object)panel4);
			((Control)panel2).get_Controls().Add((Control)(object)panel8);
			((Control)panel2).get_Controls().Add((Control)(object)panel7);
			((Control)panel2).get_Controls().Add((Control)(object)cmdMPChelp);
			((Control)panel2).get_Controls().Add((Control)(object)groupBox1);
			((Control)panel2).get_Controls().Add((Control)(object)label1);
			((Control)panel2).get_Controls().Add((Control)(object)cmdMakeShort_CatCorrns);
			((Control)panel2).set_Location(new Point(0, 0));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(503, 643));
			((Control)panel2).set_TabIndex(0);
			((Control)panel14).set_BackColor(Color.Cornsilk);
			panel14.set_BorderStyle((BorderStyle)2);
			((Control)panel14).get_Controls().Add((Control)(object)label20);
			((Control)panel14).get_Controls().Add((Control)(object)label99);
			((Control)panel14).get_Controls().Add((Control)(object)label100);
			((Control)panel14).get_Controls().Add((Control)(object)label19);
			((Control)panel14).get_Controls().Add((Control)(object)label18);
			((Control)panel14).get_Controls().Add((Control)(object)txtMPC_MEA);
			((Control)panel14).get_Controls().Add((Control)(object)txtMPC_CON);
			((Control)panel14).set_Location(new Point(7, 526));
			((Control)panel14).set_Name("panel14");
			((Control)panel14).set_Size(new Size(483, 109));
			((Control)panel14).set_TabIndex(17);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(30, 3));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(419, 17));
			((Control)label20).set_TabIndex(21);
			((Control)label20).set_Text("Names for the Submitter && Measurers fields in the report");
			((Control)label99).set_AutoSize(true);
			((Control)label99).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label99).set_ForeColor(Color.Indigo);
			((Control)label99).set_Location(new Point(95, 25));
			((Control)label99).set_Name("label99");
			((Control)label99).set_Size(new Size(353, 13));
			((Control)label99).set_TabIndex(20);
			((Control)label99).set_Text("Required.  Name of the person submitting the report.   Initial + family name");
			((Control)label100).set_AutoSize(true);
			((Control)label100).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label100).set_ForeColor(Color.Indigo);
			((Control)label100).set_Location(new Point(95, 66));
			((Control)label100).set_Name("label100");
			((Control)label100).set_Size(new Size(381, 13));
			((Control)label100).set_TabIndex(19);
			((Control)label100).set_Text("Required.  Regional coordinator names.  Initial + family name, comma separated");
			label100.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(11, 21));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(77, 17));
			((Control)label19).set_TabIndex(18);
			((Control)label19).set_Text("Submitter");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(11, 63));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(84, 17));
			((Control)label18).set_TabIndex(17);
			((Control)label18).set_Text("Measurers");
			((Control)txtMPC_MEA).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "MPC_MEA", true, (DataSourceUpdateMode)1));
			((Control)txtMPC_MEA).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMPC_MEA).set_Location(new Point(11, 81));
			((TextBoxBase)txtMPC_MEA).set_MaxLength(150);
			((Control)txtMPC_MEA).set_Name("txtMPC_MEA");
			((Control)txtMPC_MEA).set_Size(new Size(456, 20));
			((Control)txtMPC_MEA).set_TabIndex(15);
			((Control)txtMPC_MEA).set_Text(Settings.Default.MPC_MEA);
			toolTip1.SetToolTip((Control)(object)txtMPC_MEA, "Example    J. Smith, R. Jones.  Any number of names");
			((Control)txtMPC_CON).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "MPC_CON", true, (DataSourceUpdateMode)1));
			((Control)txtMPC_CON).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMPC_CON).set_Location(new Point(11, 40));
			((TextBoxBase)txtMPC_CON).set_MaxLength(150);
			((Control)txtMPC_CON).set_Name("txtMPC_CON");
			((Control)txtMPC_CON).set_Size(new Size(456, 20));
			((Control)txtMPC_CON).set_TabIndex(14);
			((Control)txtMPC_CON).set_Text(Settings.Default.MPC_CON);
			toolTip1.SetToolTip((Control)(object)txtMPC_CON, "Example    J. Smith.  [Only one name]");
			((Control)panel4).set_BackColor(Color.LightYellow);
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)label22);
			((Control)panel4).get_Controls().Add((Control)(object)txtADESversion);
			((Control)panel4).get_Controls().Add((Control)(object)label8);
			((Control)panel4).get_Controls().Add((Control)(object)label7);
			((Control)panel4).get_Controls().Add((Control)(object)cmdADESfields);
			((Control)panel4).get_Controls().Add((Control)(object)cmdGetADES);
			((Control)panel4).set_Location(new Point(341, 309));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(153, 208));
			((Control)panel4).set_TabIndex(2);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_BackColor(Color.FloralWhite);
			label22.set_BorderStyle((BorderStyle)2);
			((Control)label22).set_ForeColor(Color.Maroon);
			((Control)label22).set_Location(new Point(23, 20));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(103, 28));
			((Control)label22).set_TabIndex(16);
			((Control)label22).set_Text("Astrometry Data\r\nExchange Standard");
			label22.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)txtADESversion).set_BackColor(Color.Yellow);
			((Control)txtADESversion).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "ADES_version", true, (DataSourceUpdateMode)1));
			((Control)txtADESversion).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)txtADESversion).set_Location(new Point(52, 78));
			((Control)txtADESversion).set_Name("txtADESversion");
			((TextBoxBase)txtADESversion).set_ReadOnly(true);
			((Control)txtADESversion).set_Size(new Size(44, 20));
			((Control)txtADESversion).set_TabIndex(15);
			((Control)txtADESversion).set_Text(Settings.Default.ADES_version);
			txtADESversion.set_TextAlign((HorizontalAlignment)2);
			toolTip1.SetToolTip((Control)(object)txtADESversion, componentResourceManager.GetString("txtADESversion.ToolTip"));
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(13, 61));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(122, 13));
			((Control)label8).set_TabIndex(14);
			((Control)label8).set_Text("Version ID for report");
			label8.set_TextAlign(ContentAlignment.TopRight);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(49, 3));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(49, 17));
			((Control)label7).set_TabIndex(13);
			((Control)label7).set_Text("ADES");
			((Control)cmdADESfields).set_BackColor(Color.Khaki);
			((Control)cmdADESfields).set_Location(new Point(36, 174));
			((Control)cmdADESfields).set_Name("cmdADESfields");
			((Control)cmdADESfields).set_Size(new Size(77, 23));
			((Control)cmdADESfields).set_TabIndex(1);
			((Control)cmdADESfields).set_Text("Field values");
			((ButtonBase)cmdADESfields).set_UseVisualStyleBackColor(false);
			((Control)cmdADESfields).set_Visible(false);
			((Control)cmdADESfields).add_Click((EventHandler)cmdADESfields_Click);
			((Control)cmdGetADES).set_BackColor(Color.Wheat);
			((Control)cmdGetADES).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGetADES).set_Location(new Point(28, 117));
			((Control)cmdGetADES).set_Name("cmdGetADES");
			((Control)cmdGetADES).set_Size(new Size(93, 53));
			((Control)cmdGetADES).set_TabIndex(0);
			((Control)cmdGetADES).set_Text("Download\r\nADES\r\nspecification");
			((ButtonBase)cmdGetADES).set_UseVisualStyleBackColor(false);
			((Control)cmdGetADES).add_Click((EventHandler)cmdGetADES_Click);
			((Control)panel8).set_BackColor(Color.SeaShell);
			panel8.set_BorderStyle((BorderStyle)2);
			((Control)panel8).get_Controls().Add((Control)(object)cmdAddMPC);
			((Control)panel8).get_Controls().Add((Control)(object)textBox1);
			((Control)panel8).get_Controls().Add((Control)(object)label15);
			((Control)panel8).get_Controls().Add((Control)(object)cmdClear);
			((Control)panel8).set_Location(new Point(174, 309));
			((Control)panel8).set_Name("panel8");
			((Control)panel8).set_Size(new Size(153, 208));
			((Control)panel8).set_TabIndex(11);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_BackColor(Color.SeaShell);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_ForeColor(Color.Black);
			((Control)label15).set_Location(new Point(24, 3));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(100, 34));
			((Control)label15).set_TabIndex(4);
			((Control)label15).set_Text("Occult\r\nmaintenance");
			label15.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdClear).set_BackColor(Color.Pink);
			((Control)cmdClear).set_ForeColor(Color.DarkBlue);
			((Control)cmdClear).set_Location(new Point(23, 158));
			((Control)cmdClear).set_Name("cmdClear");
			((Control)cmdClear).set_Size(new Size(123, 42));
			((Control)cmdClear).set_TabIndex(1);
			((Control)cmdClear).set_Text("Clear MPC info for \r\na submission date");
			((ButtonBase)cmdClear).set_UseVisualStyleBackColor(false);
			((Control)cmdClear).set_Visible(false);
			((Control)cmdClear).add_Click((EventHandler)cmdClear_Click);
			((Control)cmdAddMPC).set_BackColor(Color.NavajoWhite);
			((Control)cmdAddMPC).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddMPC).set_Location(new Point(8, 124));
			((Control)cmdAddMPC).set_Name("cmdAddMPC");
			((Control)cmdAddMPC).set_Size(new Size(133, 69));
			((Control)cmdAddMPC).set_TabIndex(0);
			((Control)cmdAddMPC).set_Text("Add MPC publication details \r\nto the Observations file");
			((ButtonBase)cmdAddMPC).set_UseVisualStyleBackColor(false);
			((Control)cmdAddMPC).add_Click((EventHandler)cmdAddMPC_Click);
			((Control)panel7).set_BackColor(Color.LightCyan);
			panel7.set_BorderStyle((BorderStyle)2);
			((Control)panel7).get_Controls().Add((Control)(object)cmdMPC_ServiceDesk);
			((Control)panel7).get_Controls().Add((Control)(object)cmdValidateStatus);
			((Control)panel7).get_Controls().Add((Control)(object)label13);
			((Control)panel7).get_Controls().Add((Control)(object)MPC_Explorer);
			((Control)panel7).get_Controls().Add((Control)(object)cmdMPCstatus);
			((Control)panel7).get_Controls().Add((Control)(object)cmdMPCsubmit);
			((Control)panel7).get_Controls().Add((Control)(object)cmdMPCtest);
			((Control)panel7).set_Location(new Point(7, 309));
			((Control)panel7).set_Name("panel7");
			((Control)panel7).set_Size(new Size(153, 208));
			((Control)panel7).set_TabIndex(10);
			((Control)cmdValidateStatus).set_BackColor(Color.Turquoise);
			((Control)cmdValidateStatus).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdValidateStatus).set_Location(new Point(7, 55));
			((Control)cmdValidateStatus).set_Name("cmdValidateStatus");
			((Control)cmdValidateStatus).set_Size(new Size(134, 27));
			((Control)cmdValidateStatus).set_TabIndex(4);
			((Control)cmdValidateStatus).set_Text("2. Validation status");
			((ButtonBase)cmdValidateStatus).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdValidateStatus).set_UseVisualStyleBackColor(false);
			((Control)cmdValidateStatus).add_Click((EventHandler)cmdValidateStatus_Click);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_BackColor(Color.LightCyan);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_ForeColor(Color.Black);
			((Control)label13).set_Location(new Point(9, 3));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(131, 17));
			((Control)label13).set_TabIndex(3);
			((Control)label13).set_Text("Submit && monitor");
			((Control)MPC_Explorer).set_BackColor(Color.LimeGreen);
			((Control)MPC_Explorer).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)MPC_Explorer).set_Location(new Point(7, 142));
			((Control)MPC_Explorer).set_Name("MPC_Explorer");
			((Control)MPC_Explorer).set_Size(new Size(134, 27));
			((Control)MPC_Explorer).set_TabIndex(2);
			((Control)MPC_Explorer).set_Text("5. MPC Explorer");
			((ButtonBase)MPC_Explorer).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)MPC_Explorer).set_UseVisualStyleBackColor(false);
			((Control)MPC_Explorer).add_Click((EventHandler)MPC_Explorer_Click);
			((Control)cmdMPCstatus).set_BackColor(Color.Turquoise);
			((Control)cmdMPCstatus).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMPCstatus).set_Location(new Point(7, 113));
			((Control)cmdMPCstatus).set_Name("cmdMPCstatus");
			((Control)cmdMPCstatus).set_Size(new Size(134, 27));
			((Control)cmdMPCstatus).set_TabIndex(2);
			((Control)cmdMPCstatus).set_Text("4. Submission status");
			((ButtonBase)cmdMPCstatus).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdMPCstatus).set_UseVisualStyleBackColor(false);
			((Control)cmdMPCstatus).add_Click((EventHandler)cmdMPCstatus_Click);
			((Control)cmdMPCsubmit).set_BackColor(Color.Yellow);
			((Control)cmdMPCsubmit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMPCsubmit).set_Location(new Point(7, 84));
			((Control)cmdMPCsubmit).set_Name("cmdMPCsubmit");
			((Control)cmdMPCsubmit).set_Size(new Size(134, 27));
			((Control)cmdMPCsubmit).set_TabIndex(1);
			((Control)cmdMPCsubmit).set_Text("3. Submit report");
			((ButtonBase)cmdMPCsubmit).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdMPCsubmit).set_UseVisualStyleBackColor(false);
			((Control)cmdMPCsubmit).add_Click((EventHandler)cmdMPCsubmit_Click);
			((Control)cmdMPCtest).set_BackColor(Color.PeachPuff);
			((Control)cmdMPCtest).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMPCtest).set_Location(new Point(7, 26));
			((Control)cmdMPCtest).set_Name("cmdMPCtest");
			((Control)cmdMPCtest).set_Size(new Size(134, 27));
			((Control)cmdMPCtest).set_TabIndex(0);
			((Control)cmdMPCtest).set_Text("1. Validate report");
			((ButtonBase)cmdMPCtest).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdMPCtest).set_UseVisualStyleBackColor(false);
			((Control)cmdMPCtest).add_Click((EventHandler)cmdMPCtest_Click);
			((ButtonBase)cmdMPChelp).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)cmdMPChelp).set_FlatStyle((FlatStyle)0);
			((Control)cmdMPChelp).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdMPChelp).set_Image((Image)Resources.help);
			((ButtonBase)cmdMPChelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdMPChelp).set_Location(new Point(396, 3));
			((Control)cmdMPChelp).set_Name("cmdMPChelp");
			((Control)cmdMPChelp).set_Size(new Size(85, 32));
			((Control)cmdMPChelp).set_TabIndex(9);
			((Control)cmdMPChelp).set_Text("Help");
			((ButtonBase)cmdMPChelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdMPChelp).set_UseVisualStyleBackColor(true);
			((Control)cmdMPChelp).add_Click((EventHandler)cmdMPChelp_Click);
			((Control)groupBox1).set_BackColor(Color.Aquamarine);
			((Control)groupBox1).get_Controls().Add((Control)(object)panel11);
			((Control)groupBox1).get_Controls().Add((Control)(object)panel10);
			((Control)groupBox1).get_Controls().Add((Control)(object)panel5);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdMPCreport);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(10, 41));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(488, 260));
			((Control)groupBox1).set_TabIndex(3);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Create report");
			((Control)panel11).set_BackColor(Color.BlanchedAlmond);
			panel11.set_BorderStyle((BorderStyle)2);
			((Control)panel11).get_Controls().Add((Control)(object)label14);
			((Control)panel11).get_Controls().Add((Control)(object)chkByYear);
			((Control)panel11).get_Controls().Add((Control)(object)optExtraData);
			((Control)panel11).get_Controls().Add((Control)(object)panel3);
			((Control)panel11).get_Controls().Add((Control)(object)label3);
			((Control)panel11).get_Controls().Add((Control)(object)txtStartYear);
			((Control)panel11).get_Controls().Add((Control)(object)txtEndYear);
			((Control)panel11).set_Location(new Point(3, 122));
			((Control)panel11).set_Name("panel11");
			((Control)panel11).set_Size(new Size(246, 93));
			((Control)panel11).set_TabIndex(19);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_ForeColor(Color.Maroon);
			((Control)label14).set_Location(new Point(3, 3));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(236, 13));
			((Control)label14).set_TabIndex(19);
			((Control)label14).set_Text("Output options. Normally, do not change");
			((Control)chkByYear).set_AutoSize(true);
			((Control)chkByYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkByYear).set_Location(new Point(12, 24));
			((Control)chkByYear).set_Name("chkByYear");
			((Control)chkByYear).set_Size(new Size(113, 17));
			((Control)chkByYear).set_TabIndex(6);
			((Control)chkByYear).set_Text("Output by year, for");
			((ButtonBase)chkByYear).set_UseVisualStyleBackColor(true);
			optExtraData.set_AutoCheck(false);
			((Control)optExtraData).set_AutoSize(true);
			((Control)optExtraData).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optExtraData).set_Location(new Point(12, 43));
			((Control)optExtraData).set_Name("optExtraData");
			((Control)optExtraData).set_Size(new Size(171, 17));
			((Control)optExtraData).set_TabIndex(16);
			optExtraData.set_TabStop(true);
			((Control)optExtraData).set_Text("Astrometry only, with extra data");
			((ButtonBase)optExtraData).set_UseVisualStyleBackColor(true);
			((Control)optExtraData).add_Click((EventHandler)optExtraData_Click);
			((Control)panel3).get_Controls().Add((Control)(object)label5);
			((Control)panel3).get_Controls().Add((Control)(object)label4);
			((Control)panel3).get_Controls().Add((Control)(object)optRAdec);
			((Control)panel3).get_Controls().Add((Control)(object)optXYZ);
			((Control)panel3).set_Location(new Point(21, 61));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(201, 24));
			((Control)panel3).set_TabIndex(2);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(109, 5));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(16, 13));
			((Control)label5).set_TabIndex(3);
			((Control)label5).set_Text("or");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(6, 5));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(53, 13));
			((Control)label4).set_TabIndex(2);
			((Control)label4).set_Text("Report as");
			((Control)optRAdec).set_AutoSize(true);
			((Control)optRAdec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optRAdec).set_ForeColor(Color.DimGray);
			((Control)optRAdec).set_Location(new Point(127, 3));
			((Control)optRAdec).set_Name("optRAdec");
			((Control)optRAdec).set_Size(new Size(71, 17));
			((Control)optRAdec).set_TabIndex(1);
			((Control)optRAdec).set_Text("RA/Dec");
			((ButtonBase)optRAdec).set_UseVisualStyleBackColor(true);
			optRAdec.add_CheckedChanged((EventHandler)optRAdec_CheckedChanged);
			((Control)optXYZ).set_AutoSize(true);
			optXYZ.set_Checked(true);
			((Control)optXYZ).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optXYZ).set_Location(new Point(61, 3));
			((Control)optXYZ).set_Name("optXYZ");
			((Control)optXYZ).set_Size(new Size(51, 17));
			((Control)optXYZ).set_TabIndex(0);
			optXYZ.set_TabStop(true);
			((Control)optXYZ).set_Text("x,y,z");
			((ButtonBase)optXYZ).set_UseVisualStyleBackColor(true);
			optXYZ.add_CheckedChanged((EventHandler)optXYZ_CheckedChanged);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(162, 25));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(16, 13));
			((Control)label3).set_TabIndex(13);
			((Control)label3).set_Text("to");
			((Control)txtStartYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStartYear).set_Location(new Point(128, 21));
			((Control)txtStartYear).set_Name("txtStartYear");
			((Control)txtStartYear).set_Size(new Size(32, 20));
			((Control)txtStartYear).set_TabIndex(7);
			((Control)txtStartYear).set_Text("2021");
			((Control)txtStartYear).add_KeyPress(new KeyPressEventHandler(txtStartYear_KeyPress));
			((Control)txtEndYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtEndYear).set_Location(new Point(181, 21));
			((Control)txtEndYear).set_Name("txtEndYear");
			((Control)txtEndYear).set_Size(new Size(32, 20));
			((Control)txtEndYear).set_TabIndex(8);
			((Control)txtEndYear).set_Text("2021");
			((Control)txtEndYear).add_KeyPress(new KeyPressEventHandler(txtEndYear_KeyPress));
			((Control)panel10).set_BackColor(Color.Beige);
			panel10.set_BorderStyle((BorderStyle)2);
			((Control)panel10).get_Controls().Add((Control)(object)label12);
			((Control)panel10).get_Controls().Add((Control)(object)label11);
			((Control)panel10).get_Controls().Add((Control)(object)chkAsteroidSatellites);
			((Control)panel10).get_Controls().Add((Control)(object)chkAsteroids);
			((Control)panel10).get_Controls().Add((Control)(object)label9);
			((Control)panel10).get_Controls().Add((Control)(object)chkPlanets);
			((Control)panel10).set_Location(new Point(3, 17));
			((Control)panel10).set_Name("panel10");
			((Control)panel10).set_Size(new Size(246, 101));
			((Control)panel10).set_TabIndex(18);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(52, 3));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(139, 17));
			((Control)label12).set_TabIndex(18);
			((Control)label12).set_Text("Objects to include");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_ForeColor(Color.Crimson);
			((Control)label11).set_Location(new Point(18, 70));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(221, 13));
			((Control)label11).set_TabIndex(17);
			((Control)label11).set_Text("MPC can't process these satellites - Dec 2024");
			((Control)chkAsteroidSatellites).set_AutoSize(true);
			((Control)chkAsteroidSatellites).set_Font(new Font("Microsoft Sans Serif", 9.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAsteroidSatellites).set_ForeColor(Color.Crimson);
			((Control)chkAsteroidSatellites).set_Location(new Point(6, 55));
			((Control)chkAsteroidSatellites).set_Name("chkAsteroidSatellites");
			((Control)chkAsteroidSatellites).set_Size(new Size(217, 20));
			((Control)chkAsteroidSatellites).set_TabIndex(14);
			((Control)chkAsteroidSatellites).set_Text("Satellites of Asteroids, && of Mars");
			((ButtonBase)chkAsteroidSatellites).set_UseVisualStyleBackColor(true);
			((Control)chkAsteroids).set_AutoSize(true);
			chkAsteroids.set_Checked(true);
			chkAsteroids.set_CheckState((CheckState)1);
			((Control)chkAsteroids).set_Font(new Font("Microsoft Sans Serif", 9.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAsteroids).set_ForeColor(Color.DarkGreen);
			((Control)chkAsteroids).set_Location(new Point(6, 18));
			((Control)chkAsteroids).set_Name("chkAsteroids");
			((Control)chkAsteroids).set_Size(new Size(235, 20));
			((Control)chkAsteroids).set_TabIndex(0);
			((Control)chkAsteroids).set_Text("Asteroids, Comets, Planet satellites");
			((ButtonBase)chkAsteroids).set_UseVisualStyleBackColor(true);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_ForeColor(Color.DarkViolet);
			((Control)label9).set_Location(new Point(18, 84));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(195, 13));
			((Control)label9).set_TabIndex(15);
			((Control)label9).set_Text("Pluto && its satellites temporarily excluded");
			((Control)chkPlanets).set_AutoSize(true);
			((Control)chkPlanets).set_Font(new Font("Microsoft Sans Serif", 9.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPlanets).set_ForeColor(Color.DarkGreen);
			((Control)chkPlanets).set_Location(new Point(6, 36));
			((Control)chkPlanets).set_Name("chkPlanets");
			((Control)chkPlanets).set_Size(new Size(74, 20));
			((Control)chkPlanets).set_TabIndex(1);
			((Control)chkPlanets).set_Text("Planets ");
			((ButtonBase)chkPlanets).set_UseVisualStyleBackColor(true);
			((Control)panel5).set_BackColor(Color.MistyRose);
			panel5.set_BorderStyle((BorderStyle)2);
			((Control)panel5).get_Controls().Add((Control)(object)optObserverOnly);
			((Control)panel5).get_Controls().Add((Control)(object)label10);
			((Control)panel5).get_Controls().Add((Control)(object)txtObserver);
			((Control)panel5).get_Controls().Add((Control)(object)optLatestYear);
			((Control)panel5).get_Controls().Add((Control)(object)chkExcludeNoPM);
			((Control)panel5).get_Controls().Add((Control)(object)chkObserverName);
			((Control)panel5).get_Controls().Add((Control)(object)label6);
			((Control)panel5).get_Controls().Add((Control)(object)chkIncludeAll);
			((Control)panel5).get_Controls().Add((Control)(object)cmbEndMonth);
			((Control)panel5).get_Controls().Add((Control)(object)cmbEndYear);
			((Control)panel5).get_Controls().Add((Control)(object)chkEndDateLimit);
			((Control)panel5).get_Controls().Add((Control)(object)txtAsteroid);
			((Control)panel5).get_Controls().Add((Control)(object)chkAsteroid);
			((Control)panel5).set_Location(new Point(255, 17));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(226, 198));
			((Control)panel5).set_TabIndex(3);
			optObserverOnly.set_AutoCheck(false);
			((Control)optObserverOnly).set_AutoSize(true);
			((Control)optObserverOnly).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ButtonBase)optObserverOnly).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)optObserverOnly).set_Location(new Point(174, 84));
			((Control)optObserverOnly).set_Name("optObserverOnly");
			((Control)optObserverOnly).set_Size(new Size(14, 13));
			((Control)optObserverOnly).set_TabIndex(8);
			optObserverOnly.set_TabStop(true);
			toolTip1.SetToolTip((Control)(object)optObserverOnly, "Limits the output to events where there are no other observers");
			((ButtonBase)optObserverOnly).set_UseVisualStyleBackColor(true);
			((Control)optObserverOnly).add_Click((EventHandler)optObserverOnly_Click);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(184, 77));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(40, 26));
			((Control)label10).set_TabIndex(9);
			((Control)label10).set_Text("and no\r\nothers");
			((Control)txtObserver).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtObserver).set_Location(new Point(114, 80));
			((Control)txtObserver).set_Name("txtObserver");
			((Control)txtObserver).set_Size(new Size(57, 20));
			((Control)txtObserver).set_TabIndex(7);
			optLatestYear.set_AutoCheck(false);
			((Control)optLatestYear).set_AutoSize(true);
			((Control)optLatestYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optLatestYear).set_Location(new Point(7, 161));
			((Control)optLatestYear).set_Name("optLatestYear");
			((Control)optLatestYear).set_Size(new Size(183, 17));
			((Control)optLatestYear).set_TabIndex(9);
			optLatestYear.set_TabStop(true);
			((Control)optLatestYear).set_Text("Generate test report for 2023 only");
			((ButtonBase)optLatestYear).set_UseVisualStyleBackColor(true);
			((Control)optLatestYear).add_MouseClick(new MouseEventHandler(optLatestYear_MouseClick));
			((Control)chkExcludeNoPM).set_AutoSize(true);
			((Control)chkExcludeNoPM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkExcludeNoPM).set_Location(new Point(7, 141));
			((Control)chkExcludeNoPM).set_Name("chkExcludeNoPM");
			((Control)chkExcludeNoPM).set_Size(new Size(193, 17));
			((Control)chkExcludeNoPM).set_TabIndex(5);
			((Control)chkExcludeNoPM).set_Text("Exclude stars with no proper motion");
			((ButtonBase)chkExcludeNoPM).set_UseVisualStyleBackColor(true);
			((Control)chkObserverName).set_AutoSize(true);
			((Control)chkObserverName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkObserverName).set_Location(new Point(7, 75));
			((Control)chkObserverName).set_Name("chkObserverName");
			((Control)chkObserverName).set_Size(new Size(109, 30));
			((Control)chkObserverName).set_TabIndex(6);
			((Control)chkObserverName).set_Text("Limit to observer\r\nnames containing");
			((ButtonBase)chkObserverName).set_TextAlign(ContentAlignment.MiddleCenter);
			((ButtonBase)chkObserverName).set_UseVisualStyleBackColor(true);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(66, 3));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(90, 17));
			((Control)label6).set_TabIndex(0);
			((Control)label6).set_Text("Constraints");
			((Control)chkIncludeAll).set_AutoSize(true);
			((Control)chkIncludeAll).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkIncludeAll).set_Location(new Point(7, 106));
			((Control)chkIncludeAll).set_Name("chkIncludeAll");
			((Control)chkIncludeAll).set_Size(new Size(161, 30));
			((Control)chkIncludeAll).set_TabIndex(4);
			((Control)chkIncludeAll).set_Text("Include all events \r\n[ not just unreported events ]");
			((ButtonBase)chkIncludeAll).set_UseVisualStyleBackColor(true);
			((Control)cmbEndMonth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbEndMonth).set_FormattingEnabled(true);
			cmbEndMonth.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbEndMonth).set_Location(new Point(153, 29));
			((Control)cmbEndMonth).set_Name("cmbEndMonth");
			((Control)cmbEndMonth).set_Size(new Size(42, 21));
			((Control)cmbEndMonth).set_TabIndex(3);
			((Control)cmbEndYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbEndYear).set_FormattingEnabled(true);
			((Control)cmbEndYear).set_Location(new Point(99, 30));
			((Control)cmbEndYear).set_Name("cmbEndYear");
			((Control)cmbEndYear).set_Size(new Size(48, 21));
			((Control)cmbEndYear).set_TabIndex(2);
			((Control)chkEndDateLimit).set_AutoSize(true);
			((Control)chkEndDateLimit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkEndDateLimit).set_Location(new Point(7, 24));
			((Control)chkEndDateLimit).set_Name("chkEndDateLimit");
			((Control)chkEndDateLimit).set_Size(new Size(92, 30));
			((Control)chkEndDateLimit).set_TabIndex(1);
			((Control)chkEndDateLimit).set_Text("Limit to before\r\nthe end of");
			((ButtonBase)chkEndDateLimit).set_TextAlign(ContentAlignment.MiddleCenter);
			((ButtonBase)chkEndDateLimit).set_UseVisualStyleBackColor(true);
			((Control)txtAsteroid).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAsteroid).set_Location(new Point(124, 55));
			((Control)txtAsteroid).set_Name("txtAsteroid");
			((Control)txtAsteroid).set_Size(new Size(66, 20));
			((Control)txtAsteroid).set_TabIndex(5);
			((Control)txtAsteroid).set_Text("0");
			((Control)txtAsteroid).add_KeyPress(new KeyPressEventHandler(txtAsteroid_KeyPress));
			((Control)chkAsteroid).set_AutoSize(true);
			((Control)chkAsteroid).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAsteroid).set_Location(new Point(7, 57));
			((Control)chkAsteroid).set_Name("chkAsteroid");
			((Control)chkAsteroid).set_Size(new Size(117, 17));
			((Control)chkAsteroid).set_TabIndex(4);
			((Control)chkAsteroid).set_Text("Limit to asteroid no.");
			((ButtonBase)chkAsteroid).set_UseVisualStyleBackColor(true);
			((Control)cmdMPCreport).set_BackColor(Color.SandyBrown);
			((Control)cmdMPCreport).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMPCreport).set_Location(new Point(139, 218));
			((Control)cmdMPCreport).set_Name("cmdMPCreport");
			((Control)cmdMPCreport).set_Size(new Size(211, 37));
			((Control)cmdMPCreport).set_TabIndex(10);
			((Control)cmdMPCreport).set_Text("Create a new MPC - ADES report");
			((ButtonBase)cmdMPCreport).set_UseVisualStyleBackColor(false);
			((Control)cmdMPCreport).add_Click((EventHandler)cmdMPCreport_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(166, 9));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(168, 20));
			((Control)label1).set_TabIndex(8);
			((Control)label1).set_Text("Minor Planet Center");
			((Control)cmdMakeShort_CatCorrns).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)cmdMakeShort_CatCorrns).set_Location(new Point(11, 11));
			((Control)cmdMakeShort_CatCorrns).set_Name("cmdMakeShort_CatCorrns");
			((Control)cmdMakeShort_CatCorrns).set_Size(new Size(108, 34));
			((Control)cmdMakeShort_CatCorrns).set_TabIndex(7);
			((Control)cmdMakeShort_CatCorrns).set_Text("Make short MPC \r\nDR2 updates report");
			((ButtonBase)cmdMakeShort_CatCorrns).set_UseVisualStyleBackColor(true);
			((Control)cmdMakeShort_CatCorrns).set_Visible(false);
			((Control)cmdMakeShort_CatCorrns).add_Click((EventHandler)cmdMakeShort_CatCorrns_Click);
			((Control)panel1).set_BackColor(Color.LightCyan);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)panel6);
			((Control)panel1).get_Controls().Add((Control)(object)panel13);
			((Control)panel1).get_Controls().Add((Control)(object)panel12);
			((Control)panel1).get_Controls().Add((Control)(object)label2);
			((Control)panel1).get_Controls().Add((Control)(object)button2);
			((Control)panel1).set_Location(new Point(509, 0));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(228, 643));
			((Control)panel1).set_TabIndex(1);
			((Control)panel6).set_BackColor(Color.NavajoWhite);
			panel6.set_BorderStyle((BorderStyle)2);
			((Control)panel6).get_Controls().Add((Control)(object)label21);
			((Control)panel6).get_Controls().Add((Control)(object)cmdPDSweb);
			((Control)panel6).get_Controls().Add((Control)(object)cmdPDS4Viewer);
			((Control)panel6).set_Location(new Point(15, 428));
			((Control)panel6).set_Name("panel6");
			((Control)panel6).set_Size(new Size(200, 174));
			((Control)panel6).set_TabIndex(17);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(17, 4));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(162, 34));
			((Control)label21).set_TabIndex(12);
			((Control)label21).set_Text("To view \r\nthe created PDS files");
			label21.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)cmdPDSweb).set_BackColor(Color.Aqua);
			((Control)cmdPDSweb).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPDSweb).set_ForeColor(Color.SaddleBrown);
			((Control)cmdPDSweb).set_Location(new Point(27, 112));
			((Control)cmdPDSweb).set_Name("cmdPDSweb");
			((Control)cmdPDSweb).set_Size(new Size(146, 39));
			((Control)cmdPDSweb).set_TabIndex(11);
			((Control)cmdPDSweb).set_Text("On-line PDS4 viewer guide");
			((ButtonBase)cmdPDSweb).set_UseVisualStyleBackColor(false);
			((Control)cmdPDSweb).add_Click((EventHandler)cmdPDSweb_Click);
			((Control)cmdPDS4Viewer).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdPDS4Viewer).set_Image((Image)Resources.PDS);
			((ButtonBase)cmdPDS4Viewer).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdPDS4Viewer).set_Location(new Point(27, 53));
			((Control)cmdPDS4Viewer).set_Name("cmdPDS4Viewer");
			((Control)cmdPDS4Viewer).set_Size(new Size(146, 40));
			((Control)cmdPDS4Viewer).set_TabIndex(10);
			((Control)cmdPDS4Viewer).set_Text("Download  \r\nPDS4 Viewer");
			((ButtonBase)cmdPDS4Viewer).set_TextAlign(ContentAlignment.MiddleRight);
			toolTip1.SetToolTip((Control)(object)cmdPDS4Viewer, "Opens the PDS4 viewer for independent running.\r\n\r\nIf the PDS4 viwer is not in Occult, it will be downloaded, and placed in the Occult 4 folder.");
			((ButtonBase)cmdPDS4Viewer).set_UseVisualStyleBackColor(true);
			((Control)cmdPDS4Viewer).add_Click((EventHandler)cmdPDS4Viewer_Click);
			((Control)panel13).set_BackColor(Color.LightYellow);
			panel13.set_BorderStyle((BorderStyle)2);
			((Control)panel13).get_Controls().Add((Control)(object)txtPDSversion);
			((Control)panel13).get_Controls().Add((Control)(object)label17);
			((Control)panel13).get_Controls().Add((Control)(object)lblPDSversion);
			((Control)panel13).get_Controls().Add((Control)(object)cmdDownloadHeaders);
			((Control)panel13).get_Controls().Add((Control)(object)cmdNASA_PDS);
			((Control)panel13).get_Controls().Add((Control)(object)chkTest);
			((Control)panel13).set_Location(new Point(15, 191));
			((Control)panel13).set_Name("panel13");
			((Control)panel13).set_Size(new Size(200, 196));
			((Control)panel13).set_TabIndex(16);
			((Control)txtPDSversion).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "PDS_Version", true, (DataSourceUpdateMode)1));
			((Control)txtPDSversion).set_Location(new Point(127, 85));
			((Control)txtPDSversion).set_Name("txtPDSversion");
			((Control)txtPDSversion).set_Size(new Size(39, 20));
			((Control)txtPDSversion).set_TabIndex(4);
			((Control)txtPDSversion).set_Text(Settings.Default.PDS_Version);
			txtPDSversion.set_TextAlign((HorizontalAlignment)2);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(43, 7));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(115, 17));
			((Control)label17).set_TabIndex(0);
			((Control)label17).set_Text("Build PDS files");
			((Control)lblPDSversion).set_AutoSize(true);
			((Control)lblPDSversion).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblPDSversion).set_Location(new Point(10, 89));
			((Control)lblPDSversion).set_Name("lblPDSversion");
			((Control)lblPDSversion).set_Size(new Size(115, 13));
			((Control)lblPDSversion).set_TabIndex(3);
			((Control)lblPDSversion).set_Text("Set version number as:");
			((Control)cmdDownloadHeaders).set_BackColor(Color.GreenYellow);
			((Control)cmdDownloadHeaders).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDownloadHeaders).set_Location(new Point(27, 37));
			((Control)cmdDownloadHeaders).set_Name("cmdDownloadHeaders");
			((Control)cmdDownloadHeaders).set_Size(new Size(156, 43));
			((Control)cmdDownloadHeaders).set_TabIndex(0);
			((Control)cmdDownloadHeaders).set_Text("Download latest Headers");
			((ButtonBase)cmdDownloadHeaders).set_UseVisualStyleBackColor(false);
			((Control)cmdDownloadHeaders).add_Click((EventHandler)cmdDownloadHeaders_Click);
			((Control)cmdNASA_PDS).set_BackColor(Color.SpringGreen);
			((Control)cmdNASA_PDS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdNASA_PDS).set_Location(new Point(27, 141));
			((Control)cmdNASA_PDS).set_Name("cmdNASA_PDS");
			((Control)cmdNASA_PDS).set_Size(new Size(156, 43));
			((Control)cmdNASA_PDS).set_TabIndex(1);
			((Control)cmdNASA_PDS).set_Text("Build \r\nPDS Archive files");
			((ButtonBase)cmdNASA_PDS).set_UseVisualStyleBackColor(false);
			((Control)cmdNASA_PDS).add_Click((EventHandler)cmdNASA_PDS_Click);
			((Control)chkTest).set_AutoSize(true);
			((Control)chkTest).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkTest).set_Location(new Point(7, 113));
			((Control)chkTest).set_Name("chkTest");
			((Control)chkTest).set_Size(new Size(183, 17));
			((Control)chkTest).set_TabIndex(2);
			((Control)chkTest).set_Text("Create short files for testing");
			((ButtonBase)chkTest).set_UseVisualStyleBackColor(true);
			((Control)panel12).set_BackColor(Color.PaleTurquoise);
			panel12.set_BorderStyle((BorderStyle)2);
			((Control)panel12).get_Controls().Add((Control)(object)cmdEditAuthorList);
			((Control)panel12).get_Controls().Add((Control)(object)label16);
			((Control)panel12).get_Controls().Add((Control)(object)cmdEditHeaders);
			((Control)panel12).set_Location(new Point(15, 44));
			((Control)panel12).set_Name("panel12");
			((Control)panel12).set_Size(new Size(200, 106));
			((Control)panel12).set_TabIndex(15);
			((Control)cmdEditAuthorList).set_BackColor(Color.Cornsilk);
			((Control)cmdEditAuthorList).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdEditAuthorList).set_Location(new Point(64, 61));
			((Control)cmdEditAuthorList).set_Name("cmdEditAuthorList");
			((Control)cmdEditAuthorList).set_Size(new Size(64, 34));
			((Control)cmdEditAuthorList).set_TabIndex(1);
			((Control)cmdEditAuthorList).set_Text("Authors");
			((ButtonBase)cmdEditAuthorList).set_UseVisualStyleBackColor(false);
			((Control)cmdEditAuthorList).add_Click((EventHandler)cmdEditAuthorList_Click);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(0, 0));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(202, 17));
			((Control)label16).set_TabIndex(0);
			((Control)label16).set_Text("Edit fixed files for building ");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(27, 8));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(116, 20));
			((Control)label2).set_TabIndex(9);
			((Control)label2).set_Text("NASA's PDS.");
			((Control)button2).set_BackColor(Color.LightCyan);
			((ButtonBase)button2).get_FlatAppearance().set_BorderSize(0);
			((ButtonBase)button2).set_FlatStyle((FlatStyle)0);
			((Control)button2).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)button2).set_Image((Image)Resources.help);
			((ButtonBase)button2).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)button2).set_Location(new Point(143, 2));
			((Control)button2).set_Name("button2");
			((Control)button2).set_Size(new Size(80, 33));
			((Control)button2).set_TabIndex(2);
			((Control)button2).set_Text("Help");
			((ButtonBase)button2).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)button2).set_UseVisualStyleBackColor(false);
			((Control)button2).add_Click((EventHandler)cmdHelpPDS_Click);
			toolTip1.set_AutomaticDelay(100);
			toolTip1.set_AutoPopDelay(9000);
			toolTip1.set_InitialDelay(100);
			toolTip1.set_IsBalloon(true);
			toolTip1.set_ReshowDelay(20);
			((Control)cmdMPC_ServiceDesk).set_BackColor(Color.Thistle);
			((Control)cmdMPC_ServiceDesk).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdMPC_ServiceDesk).set_Location(new Point(7, 171));
			((Control)cmdMPC_ServiceDesk).set_Name("cmdMPC_ServiceDesk");
			((Control)cmdMPC_ServiceDesk).set_Size(new Size(134, 27));
			((Control)cmdMPC_ServiceDesk).set_TabIndex(5);
			((Control)cmdMPC_ServiceDesk).set_Text("6. MPC Service desk");
			((ButtonBase)cmdMPC_ServiceDesk).set_TextAlign(ContentAlignment.MiddleLeft);
			((ButtonBase)cmdMPC_ServiceDesk).set_UseVisualStyleBackColor(false);
			((Control)cmdMPC_ServiceDesk).add_Click((EventHandler)cmdMPC_ServiceDesk_Click);
			((Control)textBox1).set_BackColor(Color.OldLace);
			((Control)textBox1).set_Location(new Point(8, 42));
			((TextBoxBase)textBox1).set_Multiline(true);
			((Control)textBox1).set_Name("textBox1");
			((Control)textBox1).set_Size(new Size(133, 74));
			((Control)textBox1).set_TabIndex(5);
			((Control)textBox1).set_Text("When a report has been submitted, the Date, and Unique event Identifiers, MUST be added to the asteroid Observations file.");
			textBox1.set_TextAlign((HorizontalAlignment)2);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Control)this).set_BackColor(Color.Black);
			((Form)this).set_ClientSize(new Size(737, 643));
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Control)this).set_Name("MPCandPDS");
			((Control)this).set_Text("MPCandPDS");
			((Form)this).add_Load((EventHandler)MPCandPDS_Load);
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel14).ResumeLayout(false);
			((Control)panel14).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)panel8).ResumeLayout(false);
			((Control)panel8).PerformLayout();
			((Control)panel7).ResumeLayout(false);
			((Control)panel7).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)panel11).ResumeLayout(false);
			((Control)panel11).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)panel10).ResumeLayout(false);
			((Control)panel10).PerformLayout();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panel6).ResumeLayout(false);
			((Control)panel6).PerformLayout();
			((Control)panel13).ResumeLayout(false);
			((Control)panel13).PerformLayout();
			((Control)panel12).ResumeLayout(false);
			((Control)panel12).PerformLayout();
			((Control)this).ResumeLayout(false);
		}
	}
}
