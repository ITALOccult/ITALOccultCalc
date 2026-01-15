using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class DownloadAddresses : Form
	{
		private string AddressFile;

		private ArrayList Addresses = new ArrayList();

		private IContainer components;

		private TextBox txtAddress;

		private Button cmdDownload;

		private Label label1;

		private Label label2;

		private ListBox lstSites;

		private Label label4;

		private Button cmdExit;

		private Button cmdHelp;

		public DownloadAddresses()
		{
			InitializeComponent();
			AddressFile = Utilities.AppPath + "\\Resource Files\\Addresses.txt";
		}

		private void DownloadAddresses_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			Addresses.Clear();
			lstSites.get_Items().Clear();
			lstSites.get_Items().Add((object)"Manually enter the download address");
			Addresses.Add("");
			if (!File.Exists(AddressFile))
			{
				return;
			}
			using (StreamReader streamReader = new StreamReader(AddressFile))
			{
				while (!streamReader.EndOfStream)
				{
					string text = streamReader.ReadLine();
					if (!(text.ToUpper().Substring(0, 7) == "<SOURCE"))
					{
						continue;
					}
					int num = text.IndexOf(">");
					if ((num > 1) & (num < text.Length - 1))
					{
						int num2 = lstSites.get_Items().Add((object)text.Substring(1, num - 1));
						Addresses.Add(text.Substring(num + 1));
						if (text.ToLower().Contains(".zip"))
						{
							lstSites.get_Items().set_Item(num2, (object)(lstSites.get_Items().get_Item(num2).ToString()!.PadRight(20) + "(Preferred)"));
						}
					}
				}
			}
			((ListControl)lstSites).set_SelectedIndex(1);
		}

		private void cmdDownload_Click(object sender, EventArgs e)
		{
			//IL_00e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			string text2 = "";
			string text3 = ((Control)txtAddress).get_Text();
			bool flag = false;
			int num = text3.LastIndexOf("/");
			text2 = text3.Substring(0, num + 1);
			text = text3.Substring(num + 1);
			if (text3.Trim().Length != 0 && text.Trim().Length != 0 && text2.Trim().Length >= 6)
			{
				if ((!text.ToLower().Contains(".zip")) ? http.DownloadHTTP(text2, text, Utilities.AppPath + "\\Downloaded Files\\Addresses.txt", unzip: false, gunzip: false, ShowMessages: false) : http.DownloadHTTP(text2, text, Utilities.AppPath + "\\Downloaded Files", unzip: true, gunzip: false, ShowMessages: false, ShowProgressbar: false))
				{
					File.Copy(Utilities.AppPath + "\\Downloaded Files\\Addresses.txt", Utilities.AppPath + "\\Resource Files\\Addresses.txt", overwrite: true);
					DownloadAddresses_Load(sender, e);
					MessageBox.Show("Address file has been downloaded", "Download successful", (MessageBoxButtons)0, (MessageBoxIcon)64);
				}
				else
				{
					MessageBox.Show("Address file has not been downloaded", "Download failed", (MessageBoxButtons)0, (MessageBoxIcon)48);
				}
			}
		}

		private void lstSites_SelectedIndexChanged(object sender, EventArgs e)
		{
			((Control)txtAddress).set_Text(Addresses[((ListControl)lstSites).get_SelectedIndex()]!.ToString());
			((TextBoxBase)txtAddress).set_ReadOnly(((ListControl)lstSites).get_SelectedIndex() != 0);
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Current reporting addresses");
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
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
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
			//IL_04af: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b9: Expected O, but got Unknown
			txtAddress = new TextBox();
			cmdDownload = new Button();
			label1 = new Label();
			label2 = new Label();
			lstSites = new ListBox();
			label4 = new Label();
			cmdExit = new Button();
			cmdHelp = new Button();
			((Control)this).SuspendLayout();
			((Control)txtAddress).set_Location(new Point(236, 90));
			((Control)txtAddress).set_Name("txtAddress");
			((Control)txtAddress).set_Size(new Size(407, 20));
			((Control)txtAddress).set_TabIndex(0);
			((Control)cmdDownload).set_Location(new Point(260, 141));
			((Control)cmdDownload).set_Name("cmdDownload");
			((Control)cmdDownload).set_Size(new Size(131, 29));
			((Control)cmdDownload).set_TabIndex(2);
			((Control)cmdDownload).set_Text("Download ");
			((ButtonBase)cmdDownload).set_UseVisualStyleBackColor(true);
			((Control)cmdDownload).add_Click((EventHandler)cmdDownload_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(244, 73));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(204, 13));
			((Control)label1).set_TabIndex(3);
			((Control)label1).set_Text("http or ftp address for the file of addresses");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(9, 73));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(152, 13));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("Select source for download file");
			((ListControl)lstSites).set_FormattingEnabled(true);
			((Control)lstSites).set_Location(new Point(12, 90));
			((Control)lstSites).set_Name("lstSites");
			((Control)lstSites).set_Size(new Size(199, 108));
			((Control)lstSites).set_TabIndex(5);
			lstSites.add_SelectedIndexChanged((EventHandler)lstSites_SelectedIndexChanged);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(12, 9));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(555, 60));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("Select a location to download the current email addresses for reporting observations. \r\n\r\nYou should update the current email adresses every few months \r\n\r\n");
			((Control)cmdExit).set_Location(new Point(452, 141));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(60, 29));
			((Control)cmdExit).set_TabIndex(11);
			((Control)cmdExit).set_Text("Exit");
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((Control)cmdHelp).set_Location(new Point(573, 141));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(46, 29));
			((Control)cmdHelp).set_TabIndex(12);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(657, 207));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)lstSites);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdDownload);
			((Control)this).get_Controls().Add((Control)(object)txtAddress);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarReportAddresses", true, (DataSourceUpdateMode)1));
			((Form)this).set_Location(Settings.Default.LocationLunarReportAddresses);
			((Control)this).set_Name("DownloadAddresses");
			((Control)this).set_Text("Download reporting addresses");
			((Form)this).add_Load((EventHandler)DownloadAddresses_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
