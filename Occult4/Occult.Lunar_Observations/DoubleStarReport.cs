using System;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class DoubleStarReport : Form
	{
		private bool ValidImage;

		internal string Observer = "";

		internal string Site = "";

		internal string DoubleStarDetails = "";

		internal string EventDateYYYYMMDD = "";

		private IContainer components;

		internal TextBox txtMag1;

		internal TextBox txtCCT;

		internal TextBox txtRV;

		internal TextBox txtPA;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		internal TextBox txtT2;

		internal TextBox txtT1;

		private Button cmdRetrieveImage;

		internal Label lblDate;

		internal Label lblStar;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withReportToolStripMenuItem;

		private ToolStripMenuItem emailToolStripMenuItem;

		private ToolStripMenuItem saveLightCurveToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Button cmdEmailDoubleReport;

		internal Label lblTime;

		private Button cmdSaveLightCurve;

		private Label label7;

		private Label label8;

		private Label label9;

		internal TextBox txt_cc_Addresses;

		internal TextBox txtMessage;

		internal PictureBox picLiMovie;

		private Button cmdGetDefaultAdddresses;

		private Button cmdSetDefaultAddresses;

		private ToolTip toolTip1;

		private CheckBox chkNoDouble;

		internal Label label10;

		private Label label12;

		internal TextBox txtAA;

		private Label label13;

		internal TextBox txtL;

		private Label label14;

		internal TextBox txtB;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem saveReportToolStripMenuItem;

		private Button cmdSaveReport;

		internal Label lblEvent;

		private ToolStripMenuItem viewReportToolStripMenuItem;

		private Button cmdViewReport;

		private Label lblT2;

		private ToolStripMenuItem copyToolStripMenuItem;

		private Label label11;

		internal TextBox txtAlt;

		private Label label15;

		internal TextBox txtIllum;

		private Label label16;

		internal TextBox txtCA;

		private Label label17;

		internal TextBox txtMoonScale;

		private Label label18;

		internal TextBox txtEmailAddress;

		private Button cmdEditEmailAddress;

		public DoubleStarReport()
		{
			InitializeComponent();
		}

		private void DoubleStarReport_Load(object sender, EventArgs e)
		{
			((Control)txtEmailAddress).set_Text("");
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			string path = Utilities.AppPath + "\\Resource Files\\addresses.txt";
			if (File.Exists(path))
			{
				using StreamReader streamReader = new StreamReader(path);
				while (!streamReader.EndOfStream)
				{
					string text = streamReader.ReadLine();
					if (text.Substring(0, 9) == "<Doubles>")
					{
						((Control)txtEmailAddress).set_Text(text.Substring(9));
						break;
					}
				}
			}
			SettxtEmails();
			((Control)txt_cc_Addresses).set_Text(Settings.Default.DoubleStarEmails);
			toolTip1.SetToolTip((Control)(object)cmdGetDefaultAdddresses, Settings.Default.DoubleStarEmails);
		}

		private void cmdRetrieveImage_Click(object sender, EventArgs e)
		{
			//IL_0056: Unknown result type (might be due to invalid IL or missing references)
			if (Clipboard.ContainsImage())
			{
				ValidImage = true;
				((Control)picLiMovie).set_Width(Clipboard.GetImage().Width);
				((Control)picLiMovie).set_Height(Clipboard.GetImage().Height);
				picLiMovie.set_Image(Clipboard.GetImage());
			}
			else
			{
				MessageBox.Show("No image on clipboard", "No image", (MessageBoxButtons)0, (MessageBoxIcon)16);
				ValidImage = false;
			}
		}

		private void saveLightCurveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveLightCurve();
		}

		private void cmdSaveLightCurve_Click(object sender, EventArgs e)
		{
			SaveLightCurve();
		}

		private void SaveLightCurve()
		{
			Settings.Default.Save_LightCurve = Output.SaveGraphic(picLiMovie.get_Image(), ((Control)lblDate).get_Text().Replace(" ", "") + "_" + ((Control)lblStar).get_Text(), Settings.Default.Save_LightCurve);
		}

		private void emailToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			if (!EmailReport())
			{
				MessageBox.Show("The report has not been emailed", "Not emailed");
			}
		}

		private void cmdEmailDoubleReport_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet.\r\n\r\nIf the internet cannot be connected, save the report using the 'Save report' button.", "No internet");
			}
			else if (!EmailReport())
			{
				MessageBox.Show("The report has not been emailed", "Not emailed");
			}
		}

		private bool EmailReport()
		{
			//IL_003c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Unknown result type (might be due to invalid IL or missing references)
			//IL_011d: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			if (((Control)txtT2).get_Enabled() && ((Control)txtT2).get_Text().Trim() == "")
			{
				MessageBox.Show("Seconds 2 - the seconds for the 2nd star, has not been specified.", "No Time 2", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return false;
			}
			if (!SaveReport())
			{
				return false;
			}
			if (((Control)txtEmailAddress).get_Text().IndexOf("@") < 0)
			{
				MessageBox.Show("There must be an email address to send the report to.", "No addresses", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return false;
			}
			if (!((Control)txt_cc_Addresses).get_Text().EndsWith(";"))
			{
				((Control)txt_cc_Addresses).set_Text(((Control)txt_cc_Addresses).get_Text() + ";");
			}
			text = Emails.Email_DoubleStarReport(CreateMessageBody() + ((Control)txtMessage).get_Text(), Subject: EventDateYYYYMMDD + ", " + ((Control)lblStar).get_Text() + " - Double Star report", IncludeImage: ValidImage);
			if (text.Length > 5)
			{
				MessageBox.Show(text, "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else
			{
				MessageBox.Show("Double star report has been sent", "Successful email", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
			if (text.Length > 5)
			{
				return false;
			}
			return true;
		}

		private string CreateMessageBody()
		{
			string text = "Double star observation report\r\n\r\n";
			text = ((Control)lblDate).get_Text() + " " + ((Control)lblTime).get_Text() + "\r\n";
			text = text + "\r\nStar = " + ((Control)lblStar).get_Text();
			text = text + "\r\nObserver = " + Observer.Substring(Observer.IndexOf(":") + 1);
			text = text + "\r\nTelescope = " + Site.Substring(Site.IndexOf(":") + 1);
			if (DoubleStarDetails.Length > 10)
			{
				text = text + "\r\n\r\nDouble star details from the XZ catalogue" + DoubleStarDetails;
			}
			text = text + "\r\n\r\nObservation details\r\nStar Mag = " + ((Control)txtMag1).get_Text().Trim();
			text = text + "\r\n   Event = " + ((Control)lblEvent).get_Text().Trim();
			text = text + "\r\n      PA = " + ((Control)txtPA).get_Text().Trim();
			text = text + "\r\n      AA = " + ((Control)txtAA).get_Text().Trim();
			text = text + "\r\n       l = " + ((Control)txtL).get_Text().Trim();
			text = text + "\r\n       b = " + ((Control)txtB).get_Text().Trim();
			text = text + "\r\n      RV = " + ((Control)txtRV).get_Text().Trim();
			text = text + "\r\n   Scale = " + ((Control)txtMoonScale).get_Text().Trim();
			text = text + "\r\n     CCT = " + ((Control)txtCCT).get_Text().Trim();
			text = text + "\r\n      CA = " + ((Control)txtCA).get_Text().Trim();
			text = text + "\r\n  %Illum = " + ((Control)txtIllum).get_Text().Trim();
			text = text + "\r\n     Alt = " + ((Control)txtAlt).get_Text().Trim();
			text = text + "\r\n      T1 = " + ((Control)txtT1).get_Text().Trim();
			text = ((!chkNoDouble.get_Checked()) ? (text + "\r\n   T2-T1 = " + ((Control)txtT2).get_Text().Trim()) : (text + "\r\n      *** No step event detected. ***"));
			return text + "\r\n   * * * * *\r\n";
		}

		private void txtAddresses_TextChanged(object sender, EventArgs e)
		{
			int selectionStart = ((TextBoxBase)txt_cc_Addresses).get_SelectionStart();
			if (((Control)txt_cc_Addresses).get_Text().Contains(","))
			{
				((Control)txt_cc_Addresses).set_Text(((Control)txt_cc_Addresses).get_Text().Replace(",", ";"));
			}
			if (((Control)txt_cc_Addresses).get_Text().Contains(":"))
			{
				((Control)txt_cc_Addresses).set_Text(((Control)txt_cc_Addresses).get_Text().Replace(":", ";"));
			}
			if (((Control)txt_cc_Addresses).get_Text().Contains("\r\n"))
			{
				((Control)txt_cc_Addresses).set_Text(((Control)txt_cc_Addresses).get_Text().Replace("\r\n", ";"));
			}
			if (((Control)txt_cc_Addresses).get_Text().Contains(";;"))
			{
				((Control)txt_cc_Addresses).set_Text(((Control)txt_cc_Addresses).get_Text().Replace(";;", ";"));
			}
			((TextBoxBase)txt_cc_Addresses).set_SelectionStart(selectionStart);
		}

		private void cmdSetDefaultAddresses_Click(object sender, EventArgs e)
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to replace the following Default addresses with the present addresses? \r\n\r\n" + Settings.Default.DoubleStarEmails, "Verify replace addresses", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				Settings.Default.DoubleStarEmails = ((Control)txt_cc_Addresses).get_Text();
				toolTip1.SetToolTip((Control)(object)cmdGetDefaultAdddresses, Settings.Default.DoubleStarEmails);
			}
		}

		private void cmdGetDefaultAdddresses_Click(object sender, EventArgs e)
		{
			//IL_0026: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to replace all current addresses with the following addresses? \r\n\r\n" + Settings.Default.DoubleStarEmails, "Verify replace addresses", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				((Control)txt_cc_Addresses).set_Text(Settings.Default.DoubleStarEmails);
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Double star report");
		}

		private void chkNoDouble_CheckedChanged(object sender, EventArgs e)
		{
			if (chkNoDouble.get_Checked())
			{
				((Control)txtT2).set_Text("");
				((Control)txtT2).set_Enabled(false);
			}
			else
			{
				((Control)txtT2).set_Enabled(true);
			}
		}

		private void saveReportToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			if (!SaveReport())
			{
				MessageBox.Show("The report has not been saved", "Not saved");
			}
		}

		private void cmdSaveReport_Click(object sender, EventArgs e)
		{
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			if (!SaveReport())
			{
				MessageBox.Show("The report has not been saved", "Not saved");
			}
		}

		private bool SaveReport()
		{
			//IL_0036: Unknown result type (might be due to invalid IL or missing references)
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0044: Expected O, but got Unknown
			//IL_0082: Unknown result type (might be due to invalid IL or missing references)
			//IL_0088: Invalid comparison between Unknown and I4
			if (((Control)txtT2).get_Enabled() && ((Control)txtT2).get_Text().Trim() == "")
			{
				MessageBox.Show("Seconds 2 - the seconds for the 2nd star, has not been specified.", "No Time 2", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return false;
			}
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_InitialDirectory(Utilities.AppPath + "\\Observations\\Doubles");
			((FileDialog)val).set_FileName(EventDateYYYYMMDD + "_" + ((Control)lblStar).get_Text());
			val.set_OverwritePrompt(true);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				string text = Path.GetDirectoryName(((FileDialog)val).get_FileName()) + "\\" + Path.GetFileNameWithoutExtension(((FileDialog)val).get_FileName());
				using (StreamWriter streamWriter = new StreamWriter(text + ".txt", append: false))
				{
					streamWriter.Write(CreateMessageBody() + ((Control)txtMessage).get_Text());
				}
				if (ValidImage)
				{
					picLiMovie.get_Image().Save(text + ".png", ImageFormat.Png);
				}
				return true;
			}
			return false;
		}

		public void AutoSaveReport(string FileName)
		{
			using StreamWriter streamWriter = new StreamWriter(FileName, append: false);
			streamWriter.Write(CreateMessageBody());
		}

		private void viewReportToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show(CreateMessageBody() + ((Control)txtMessage).get_Text(), "Text of double star report");
		}

		private void cmdViewReport_Click(object sender, EventArgs e)
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			MessageBox.Show(CreateMessageBody() + ((Control)txtMessage).get_Text(), "Text of double star report");
		}

		private void txtT2_TextChanged(object sender, EventArgs e)
		{
			DisplayT2();
		}

		private void txtT2_Leave(object sender, EventArgs e)
		{
			DisplayT2();
		}

		private void DisplayT2()
		{
			if (!double.TryParse(((Control)txtT1).get_Text(), out var result))
			{
				result = 0.0;
			}
			if (!double.TryParse(((Control)txtT2).get_Text(), out var result2))
			{
				result2 = 0.0;
			}
			double num;
			for (num = result + result2; num < 0.0; num += 60.0)
			{
			}
			while (num >= 60.0)
			{
				num -= 60.0;
			}
			((Control)lblT2).set_Text(string.Format("T (faint) secs = {0,1:F3}", num));
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CreateMessageBody() + ((Control)txtMessage).get_Text());
		}

		private void cmdEditEmailAddress_Click(object sender, EventArgs e)
		{
			((TextBoxBase)txtEmailAddress).set_ReadOnly(!((TextBoxBase)txtEmailAddress).get_ReadOnly());
			SettxtEmails();
		}

		private void SettxtEmails()
		{
			if (((TextBoxBase)txtEmailAddress).get_ReadOnly())
			{
				((Control)txtEmailAddress).set_BackColor(Color.LightSalmon);
				((Control)cmdEditEmailAddress).set_Text("Edit address");
			}
			else
			{
				((Control)txtEmailAddress).set_BackColor(Color.LightGreen);
				((Control)cmdEditEmailAddress).set_Text("Lock address");
			}
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
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Expected O, but got Unknown
			//IL_00fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Expected O, but got Unknown
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0134: Expected O, but got Unknown
			//IL_0135: Unknown result type (might be due to invalid IL or missing references)
			//IL_013f: Expected O, but got Unknown
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Expected O, but got Unknown
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Expected O, but got Unknown
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_016b: Expected O, but got Unknown
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0176: Expected O, but got Unknown
			//IL_0177: Unknown result type (might be due to invalid IL or missing references)
			//IL_0181: Expected O, but got Unknown
			//IL_0182: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Expected O, but got Unknown
			//IL_0193: Unknown result type (might be due to invalid IL or missing references)
			//IL_019d: Expected O, but got Unknown
			//IL_019e: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a8: Expected O, but got Unknown
			//IL_01a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b3: Expected O, but got Unknown
			//IL_01b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01be: Expected O, but got Unknown
			//IL_01bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c9: Expected O, but got Unknown
			//IL_01ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d4: Expected O, but got Unknown
			//IL_01d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01df: Expected O, but got Unknown
			//IL_01e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ea: Expected O, but got Unknown
			//IL_01eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f5: Expected O, but got Unknown
			//IL_01f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0200: Expected O, but got Unknown
			//IL_0201: Unknown result type (might be due to invalid IL or missing references)
			//IL_020b: Expected O, but got Unknown
			//IL_020c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0216: Expected O, but got Unknown
			//IL_0217: Unknown result type (might be due to invalid IL or missing references)
			//IL_0221: Expected O, but got Unknown
			//IL_0222: Unknown result type (might be due to invalid IL or missing references)
			//IL_022c: Expected O, but got Unknown
			//IL_022d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0237: Expected O, but got Unknown
			//IL_0238: Unknown result type (might be due to invalid IL or missing references)
			//IL_0242: Expected O, but got Unknown
			//IL_0243: Unknown result type (might be due to invalid IL or missing references)
			//IL_024d: Expected O, but got Unknown
			//IL_024e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0258: Expected O, but got Unknown
			//IL_0259: Unknown result type (might be due to invalid IL or missing references)
			//IL_0263: Expected O, but got Unknown
			//IL_0264: Unknown result type (might be due to invalid IL or missing references)
			//IL_026e: Expected O, but got Unknown
			//IL_026f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0279: Expected O, but got Unknown
			//IL_027a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0284: Expected O, but got Unknown
			//IL_0285: Unknown result type (might be due to invalid IL or missing references)
			//IL_028f: Expected O, but got Unknown
			//IL_0290: Unknown result type (might be due to invalid IL or missing references)
			//IL_029a: Expected O, but got Unknown
			//IL_029b: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a5: Expected O, but got Unknown
			//IL_1f04: Unknown result type (might be due to invalid IL or missing references)
			//IL_1f0e: Expected O, but got Unknown
			components = new Container();
			txtMag1 = new TextBox();
			txtCCT = new TextBox();
			txtRV = new TextBox();
			txtPA = new TextBox();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			txtT2 = new TextBox();
			txtT1 = new TextBox();
			cmdRetrieveImage = new Button();
			lblDate = new Label();
			lblStar = new Label();
			menuStrip1 = new MenuStrip();
			withReportToolStripMenuItem = new ToolStripMenuItem();
			emailToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			viewReportToolStripMenuItem = new ToolStripMenuItem();
			saveReportToolStripMenuItem = new ToolStripMenuItem();
			saveLightCurveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdEmailDoubleReport = new Button();
			lblTime = new Label();
			cmdSaveLightCurve = new Button();
			txt_cc_Addresses = new TextBox();
			txtMessage = new TextBox();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			cmdGetDefaultAdddresses = new Button();
			cmdSetDefaultAddresses = new Button();
			toolTip1 = new ToolTip(components);
			chkNoDouble = new CheckBox();
			label10 = new Label();
			label12 = new Label();
			txtAA = new TextBox();
			label13 = new Label();
			txtL = new TextBox();
			label14 = new Label();
			txtB = new TextBox();
			cmdSaveReport = new Button();
			lblEvent = new Label();
			cmdViewReport = new Button();
			lblT2 = new Label();
			picLiMovie = new PictureBox();
			label11 = new Label();
			txtAlt = new TextBox();
			label15 = new Label();
			txtIllum = new TextBox();
			label16 = new Label();
			txtCA = new TextBox();
			label17 = new Label();
			txtMoonScale = new TextBox();
			txtEmailAddress = new TextBox();
			label18 = new Label();
			cmdEditEmailAddress = new Button();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)picLiMovie).BeginInit();
			((Control)this).SuspendLayout();
			((Control)txtMag1).set_Location(new Point(86, 94));
			((Control)txtMag1).set_Name("txtMag1");
			((TextBoxBase)txtMag1).set_ReadOnly(true);
			((Control)txtMag1).set_Size(new Size(53, 20));
			((Control)txtMag1).set_TabIndex(6);
			((Control)txtCCT).set_Location(new Point(131, 234));
			((Control)txtCCT).set_Name("txtCCT");
			((TextBoxBase)txtCCT).set_ReadOnly(true);
			((Control)txtCCT).set_Size(new Size(53, 20));
			((Control)txtCCT).set_TabIndex(26);
			((Control)txtRV).set_Location(new Point(36, 234));
			((Control)txtRV).set_Name("txtRV");
			((TextBoxBase)txtRV).set_ReadOnly(true);
			((Control)txtRV).set_Size(new Size(53, 20));
			((Control)txtRV).set_TabIndex(24);
			((Control)txtPA).set_Location(new Point(36, 122));
			((Control)txtPA).set_Name("txtPA");
			((TextBoxBase)txtPA).set_ReadOnly(true);
			((Control)txtPA).set_Size(new Size(53, 20));
			((Control)txtPA).set_TabIndex(8);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(30, 97));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(50, 13));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("Star Mag");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(6, 302));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(119, 13));
			((Control)label2).set_TabIndex(29);
			((Control)label2).set_Text("T (faint) - T (bright) secs");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(50, 275));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(74, 13));
			((Control)label3).set_TabIndex(27);
			((Control)label3).set_Text("T (bright) secs");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(101, 238));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(28, 13));
			((Control)label4).set_TabIndex(25);
			((Control)label4).set_Text("CCT");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(12, 238));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(22, 13));
			((Control)label5).set_TabIndex(23);
			((Control)label5).set_Text("RV");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(7, 126));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(27, 13));
			((Control)label6).set_TabIndex(7);
			((Control)label6).set_Text("P.A.");
			((Control)txtT2).set_Location(new Point(126, 299));
			((Control)txtT2).set_Name("txtT2");
			((Control)txtT2).set_Size(new Size(53, 20));
			((Control)txtT2).set_TabIndex(30);
			((Control)txtT2).add_TextChanged((EventHandler)txtT2_TextChanged);
			((Control)txtT2).add_Leave((EventHandler)txtT2_Leave);
			((Control)txtT1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtT1).set_Location(new Point(126, 271));
			((Control)txtT1).set_Name("txtT1");
			((TextBoxBase)txtT1).set_ReadOnly(true);
			((Control)txtT1).set_Size(new Size(53, 20));
			((Control)txtT1).set_TabIndex(28);
			((Control)cmdRetrieveImage).set_Location(new Point(33, 385));
			((Control)cmdRetrieveImage).set_Name("cmdRetrieveImage");
			((Control)cmdRetrieveImage).set_Size(new Size(121, 39));
			((Control)cmdRetrieveImage).set_TabIndex(34);
			((Control)cmdRetrieveImage).set_Text("Retrieve LiMovie plot from the Clipboard");
			((ButtonBase)cmdRetrieveImage).set_UseVisualStyleBackColor(true);
			((Control)cmdRetrieveImage).add_Click((EventHandler)cmdRetrieveImage_Click);
			((Control)lblDate).set_AutoSize(true);
			((Control)lblDate).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblDate).set_Location(new Point(12, 32));
			((Control)lblDate).set_Name("lblDate");
			((Control)lblDate).set_Size(new Size(44, 20));
			((Control)lblDate).set_TabIndex(1);
			((Control)lblDate).set_Text("Date");
			((Control)lblStar).set_AutoSize(true);
			((Control)lblStar).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblStar).set_Location(new Point(12, 60));
			((Control)lblStar).set_Name("lblStar");
			((Control)lblStar).set_Size(new Size(39, 20));
			((Control)lblStar).set_TabIndex(3);
			((Control)lblStar).set_Text("Star");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withReportToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(969, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withReportToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)emailToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)viewReportToolStripMenuItem,
				(ToolStripItem)saveReportToolStripMenuItem,
				(ToolStripItem)saveLightCurveToolStripMenuItem
			});
			((ToolStripItem)withReportToolStripMenuItem).set_Name("withReportToolStripMenuItem");
			((ToolStripItem)withReportToolStripMenuItem).set_Size(new Size(116, 20));
			((ToolStripItem)withReportToolStripMenuItem).set_Text("with Report...         ");
			((ToolStripItem)emailToolStripMenuItem).set_Image((Image)Resources.mail);
			((ToolStripItem)emailToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)emailToolStripMenuItem).set_Name("emailToolStripMenuItem");
			((ToolStripItem)emailToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)emailToolStripMenuItem).set_Text("Email report && light curve");
			((ToolStripItem)emailToolStripMenuItem).add_Click((EventHandler)emailToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(207, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)viewReportToolStripMenuItem).set_Image((Image)Resources.EYE);
			((ToolStripItem)viewReportToolStripMenuItem).set_Name("viewReportToolStripMenuItem");
			((ToolStripItem)viewReportToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)viewReportToolStripMenuItem).set_Text("View report text");
			((ToolStripItem)viewReportToolStripMenuItem).add_Click((EventHandler)viewReportToolStripMenuItem_Click);
			((ToolStripItem)saveReportToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveReportToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveReportToolStripMenuItem).set_Name("saveReportToolStripMenuItem");
			saveReportToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveReportToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)saveReportToolStripMenuItem).set_Text("Save report text");
			((ToolStripItem)saveReportToolStripMenuItem).add_Click((EventHandler)saveReportToolStripMenuItem_Click);
			((ToolStripItem)saveLightCurveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveLightCurveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveLightCurveToolStripMenuItem).set_Name("saveLightCurveToolStripMenuItem");
			((ToolStripItem)saveLightCurveToolStripMenuItem).set_Size(new Size(210, 22));
			((ToolStripItem)saveLightCurveToolStripMenuItem).set_Text("Save light curve");
			((ToolStripItem)saveLightCurveToolStripMenuItem).add_Click((EventHandler)saveLightCurveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(87, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help         ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdEmailDoubleReport).set_Location(new Point(705, 625));
			((Control)cmdEmailDoubleReport).set_Name("cmdEmailDoubleReport");
			((Control)cmdEmailDoubleReport).set_Size(new Size(121, 39));
			((Control)cmdEmailDoubleReport).set_TabIndex(44);
			((Control)cmdEmailDoubleReport).set_Text("Save and Email report");
			((ButtonBase)cmdEmailDoubleReport).set_UseVisualStyleBackColor(true);
			((Control)cmdEmailDoubleReport).add_Click((EventHandler)cmdEmailDoubleReport_Click);
			((Control)lblTime).set_AutoSize(true);
			((Control)lblTime).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblTime).set_Location(new Point(119, 34));
			((Control)lblTime).set_Name("lblTime");
			((Control)lblTime).set_Size(new Size(39, 17));
			((Control)lblTime).set_TabIndex(2);
			((Control)lblTime).set_Text("Time");
			((Control)cmdSaveLightCurve).set_Location(new Point(40, 430));
			((Control)cmdSaveLightCurve).set_Name("cmdSaveLightCurve");
			((Control)cmdSaveLightCurve).set_Size(new Size(106, 30));
			((Control)cmdSaveLightCurve).set_TabIndex(35);
			((Control)cmdSaveLightCurve).set_Text("Save light curve");
			((ButtonBase)cmdSaveLightCurve).set_UseVisualStyleBackColor(true);
			((Control)cmdSaveLightCurve).add_Click((EventHandler)cmdSaveLightCurve_Click);
			((Control)txt_cc_Addresses).set_Location(new Point(589, 509));
			((TextBoxBase)txt_cc_Addresses).set_Multiline(true);
			((Control)txt_cc_Addresses).set_Name("txt_cc_Addresses");
			((Control)txt_cc_Addresses).set_Size(new Size(266, 94));
			((Control)txt_cc_Addresses).set_TabIndex(39);
			((Control)txt_cc_Addresses).add_TextChanged((EventHandler)txtAddresses_TextChanged);
			((Control)txtMessage).set_Location(new Point(16, 488));
			((TextBoxBase)txtMessage).set_Multiline(true);
			((Control)txtMessage).set_Name("txtMessage");
			((Control)txtMessage).set_Size(new Size(520, 169));
			((Control)txtMessage).set_TabIndex(37);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(595, 494));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(252, 13));
			((Control)label7).set_TabIndex(38);
			((Control)label7).set_Text("cc Email addresses. Separate with a semicolon (  ;  )");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(13, 472));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(77, 13));
			((Control)label8).set_TabIndex(36);
			((Control)label8).set_Text("Email message");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(602, 604));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(222, 13));
			((Control)label9).set_TabIndex(40);
			((Control)label9).set_Text("[ a BCC copy will be sent to your Email Inbox ]");
			((Control)cmdGetDefaultAdddresses).set_Location(new Point(863, 513));
			((Control)cmdGetDefaultAdddresses).set_Name("cmdGetDefaultAdddresses");
			((Control)cmdGetDefaultAdddresses).set_Size(new Size(84, 34));
			((Control)cmdGetDefaultAdddresses).set_TabIndex(41);
			((Control)cmdGetDefaultAdddresses).set_Text("Get default \r\ncc addresses");
			((ButtonBase)cmdGetDefaultAdddresses).set_UseVisualStyleBackColor(true);
			((Control)cmdGetDefaultAdddresses).add_Click((EventHandler)cmdGetDefaultAdddresses_Click);
			((Control)cmdSetDefaultAddresses).set_Location(new Point(863, 564));
			((Control)cmdSetDefaultAddresses).set_Name("cmdSetDefaultAddresses");
			((Control)cmdSetDefaultAddresses).set_Size(new Size(84, 34));
			((Control)cmdSetDefaultAddresses).set_TabIndex(42);
			((Control)cmdSetDefaultAddresses).set_Text("Set as default cc addresses");
			((ButtonBase)cmdSetDefaultAddresses).set_UseVisualStyleBackColor(true);
			((Control)cmdSetDefaultAddresses).add_Click((EventHandler)cmdSetDefaultAddresses_Click);
			((Control)chkNoDouble).set_AutoSize(true);
			chkNoDouble.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkNoDouble).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkNoDouble).set_Location(new Point(79, 342));
			((Control)chkNoDouble).set_Name("chkNoDouble");
			((Control)chkNoDouble).set_Size(new Size(100, 19));
			((Control)chkNoDouble).set_TabIndex(33);
			((Control)chkNoDouble).set_Text("No step event");
			((ButtonBase)chkNoDouble).set_UseVisualStyleBackColor(true);
			chkNoDouble.add_CheckedChanged((EventHandler)chkNoDouble_CheckedChanged);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 11f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(22, 340));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(46, 18));
			((Control)label10).set_TabIndex(32);
			((Control)label10).set_Text("- or -");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(102, 126));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(27, 13));
			((Control)label12).set_TabIndex(9);
			((Control)label12).set_Text("A.A.");
			((Control)txtAA).set_Location(new Point(131, 122));
			((Control)txtAA).set_Name("txtAA");
			((TextBoxBase)txtAA).set_ReadOnly(true);
			((Control)txtAA).set_Size(new Size(53, 20));
			((Control)txtAA).set_TabIndex(10);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(25, 182));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(9, 13));
			((Control)label13).set_TabIndex(15);
			((Control)label13).set_Text("l");
			((Control)txtL).set_Location(new Point(36, 178));
			((Control)txtL).set_Name("txtL");
			((TextBoxBase)txtL).set_ReadOnly(true);
			((Control)txtL).set_Size(new Size(43, 20));
			((Control)txtL).set_TabIndex(16);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(116, 182));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(13, 13));
			((Control)label14).set_TabIndex(17);
			((Control)label14).set_Text("b");
			((Control)txtB).set_Location(new Point(131, 178));
			((Control)txtB).set_Name("txtB");
			((TextBoxBase)txtB).set_ReadOnly(true);
			((Control)txtB).set_Size(new Size(42, 20));
			((Control)txtB).set_TabIndex(18);
			((Control)cmdSaveReport).set_Location(new Point(863, 625));
			((Control)cmdSaveReport).set_Name("cmdSaveReport");
			((Control)cmdSaveReport).set_Size(new Size(84, 39));
			((Control)cmdSaveReport).set_TabIndex(45);
			((Control)cmdSaveReport).set_Text("Save report");
			((ButtonBase)cmdSaveReport).set_UseVisualStyleBackColor(true);
			((Control)cmdSaveReport).add_Click((EventHandler)cmdSaveReport_Click);
			((Control)lblEvent).set_AutoSize(true);
			((Control)lblEvent).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEvent).set_Location(new Point(119, 62));
			((Control)lblEvent).set_Name("lblEvent");
			((Control)lblEvent).set_Size(new Size(44, 17));
			((Control)lblEvent).set_TabIndex(4);
			((Control)lblEvent).set_Text("Event");
			((Control)cmdViewReport).set_Location(new Point(589, 627));
			((Control)cmdViewReport).set_Name("cmdViewReport");
			((Control)cmdViewReport).set_Size(new Size(84, 34));
			((Control)cmdViewReport).set_TabIndex(43);
			((Control)cmdViewReport).set_Text("Preview report");
			((ButtonBase)cmdViewReport).set_UseVisualStyleBackColor(true);
			((Control)cmdViewReport).add_Click((EventHandler)cmdViewReport_Click);
			((Control)lblT2).set_AutoSize(true);
			((Control)lblT2).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblT2).set_Location(new Point(64, 322));
			((Control)lblT2).set_Name("lblT2");
			((Control)lblT2).set_Size(new Size(88, 13));
			((Control)lblT2).set_TabIndex(31);
			((Control)lblT2).set_Text("T (faint) secs = T2");
			((Control)picLiMovie).set_BackColor(Color.Black);
			picLiMovie.set_BorderStyle((BorderStyle)2);
			((Control)picLiMovie).set_Location(new Point(200, 37));
			((Control)picLiMovie).set_Name("picLiMovie");
			((Control)picLiMovie).set_Size(new Size(740, 425));
			picLiMovie.set_TabIndex(13);
			picLiMovie.set_TabStop(false);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(110, 210));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(19, 13));
			((Control)label11).set_TabIndex(21);
			((Control)label11).set_Text("Alt");
			((Control)txtAlt).set_Location(new Point(131, 206));
			((Control)txtAlt).set_Name("txtAlt");
			((TextBoxBase)txtAlt).set_ReadOnly(true);
			((Control)txtAlt).set_Size(new Size(43, 20));
			((Control)txtAlt).set_TabIndex(22);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(108, 154));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(21, 13));
			((Control)label15).set_TabIndex(13);
			((Control)label15).set_Text("%ill");
			((Control)txtIllum).set_Location(new Point(131, 150));
			((Control)txtIllum).set_Name("txtIllum");
			((TextBoxBase)txtIllum).set_ReadOnly(true);
			((Control)txtIllum).set_Size(new Size(43, 20));
			((Control)txtIllum).set_TabIndex(14);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(7, 154));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(27, 13));
			((Control)label16).set_TabIndex(11);
			((Control)label16).set_Text("C.A.");
			((Control)txtCA).set_Location(new Point(36, 150));
			((Control)txtCA).set_Name("txtCA");
			((TextBoxBase)txtCA).set_ReadOnly(true);
			((Control)txtCA).set_Size(new Size(43, 20));
			((Control)txtCA).set_TabIndex(12);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(0, 210));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(34, 13));
			((Control)label17).set_TabIndex(19);
			((Control)label17).set_Text("Scale");
			((Control)txtMoonScale).set_Location(new Point(36, 206));
			((Control)txtMoonScale).set_Name("txtMoonScale");
			((TextBoxBase)txtMoonScale).set_ReadOnly(true);
			((Control)txtMoonScale).set_Size(new Size(43, 20));
			((Control)txtMoonScale).set_TabIndex(20);
			((Control)txtEmailAddress).set_Location(new Point(676, 470));
			((Control)txtEmailAddress).set_Name("txtEmailAddress");
			((TextBoxBase)txtEmailAddress).set_ReadOnly(true);
			((Control)txtEmailAddress).set_Size(new Size(189, 20));
			((Control)txtEmailAddress).set_TabIndex(46);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(547, 474));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(125, 13));
			((Control)label18).set_TabIndex(47);
			((Control)label18).set_Text("Report will be emailed to:");
			((Control)cmdEditEmailAddress).set_Location(new Point(873, 469));
			((Control)cmdEditEmailAddress).set_Name("cmdEditEmailAddress");
			((Control)cmdEditEmailAddress).set_Size(new Size(82, 22));
			((Control)cmdEditEmailAddress).set_TabIndex(48);
			((Control)cmdEditEmailAddress).set_Text("Edit address");
			((ButtonBase)cmdEditEmailAddress).set_UseVisualStyleBackColor(true);
			((Control)cmdEditEmailAddress).add_Click((EventHandler)cmdEditEmailAddress_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(969, 667));
			((Control)this).get_Controls().Add((Control)(object)cmdEditEmailAddress);
			((Control)this).get_Controls().Add((Control)(object)label18);
			((Control)this).get_Controls().Add((Control)(object)txtEmailAddress);
			((Control)this).get_Controls().Add((Control)(object)label17);
			((Control)this).get_Controls().Add((Control)(object)txtMoonScale);
			((Control)this).get_Controls().Add((Control)(object)label16);
			((Control)this).get_Controls().Add((Control)(object)txtCA);
			((Control)this).get_Controls().Add((Control)(object)label15);
			((Control)this).get_Controls().Add((Control)(object)txtIllum);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)txtAlt);
			((Control)this).get_Controls().Add((Control)(object)lblT2);
			((Control)this).get_Controls().Add((Control)(object)cmdViewReport);
			((Control)this).get_Controls().Add((Control)(object)lblEvent);
			((Control)this).get_Controls().Add((Control)(object)cmdSaveReport);
			((Control)this).get_Controls().Add((Control)(object)label14);
			((Control)this).get_Controls().Add((Control)(object)txtB);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)txtL);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)txtAA);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)chkNoDouble);
			((Control)this).get_Controls().Add((Control)(object)cmdSetDefaultAddresses);
			((Control)this).get_Controls().Add((Control)(object)cmdGetDefaultAdddresses);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)txtMessage);
			((Control)this).get_Controls().Add((Control)(object)txt_cc_Addresses);
			((Control)this).get_Controls().Add((Control)(object)cmdSaveLightCurve);
			((Control)this).get_Controls().Add((Control)(object)lblTime);
			((Control)this).get_Controls().Add((Control)(object)cmdEmailDoubleReport);
			((Control)this).get_Controls().Add((Control)(object)lblStar);
			((Control)this).get_Controls().Add((Control)(object)lblDate);
			((Control)this).get_Controls().Add((Control)(object)picLiMovie);
			((Control)this).get_Controls().Add((Control)(object)cmdRetrieveImage);
			((Control)this).get_Controls().Add((Control)(object)txtT1);
			((Control)this).get_Controls().Add((Control)(object)txtT2);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtPA);
			((Control)this).get_Controls().Add((Control)(object)txtRV);
			((Control)this).get_Controls().Add((Control)(object)txtCCT);
			((Control)this).get_Controls().Add((Control)(object)txtMag1);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarDoubleReport", true, (DataSourceUpdateMode)1));
			((Control)this).set_DoubleBuffered(true);
			((Form)this).set_Location(Settings.Default.LocationLunarDoubleReport);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("DoubleStarReport");
			((Control)this).set_Text("Double Star Report");
			((Form)this).add_Load((EventHandler)DoubleStarReport_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)picLiMovie).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
