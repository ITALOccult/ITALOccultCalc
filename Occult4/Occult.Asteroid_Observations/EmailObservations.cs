using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.IO.Compression;
using System.Windows.Forms;
using Occult.Lunar_Observations;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class EmailObservations : Form
	{
		private IContainer components;

		private Button cmdSend;

		private CheckedListBox chkLstFiles;

		private Button cmdListUnsent;

		private Button cmdListAllFiles;

		private Label label8;

		internal TextBox txtMessage;

		private TextBox txtMPC;

		private TextBox txtJPL;

		private RadioButton optMPC;

		private RadioButton optJPL;

		private Label label1;

		private Label lblBCC;

		private TextBox txtSignatureBlock;

		private Label label2;

		private TextBox txtOther;

		private RadioButton radioButton1;

		private Label label3;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Button cmdListEmail;

		public EmailObservations()
		{
			InitializeComponent();
		}

		private void EmailObservations_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			ListFiles(All: false);
			((Control)lblBCC).set_Text("A BCC copy will be sent to: " + Settings.Default.FTP_AnonymousPassword);
		}

		private void cmdListUnsent_Click(object sender, EventArgs e)
		{
			ListFiles(All: false);
		}

		private void ListFiles(bool All)
		{
			((ObjectCollection)chkLstFiles.get_Items()).Clear();
			string searchPattern = "MPC_report*.*";
			if (All)
			{
				searchPattern = "*.*";
			}
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Asteroid\\Results\\Mpc Files\\", searchPattern);
			foreach (string path in files)
			{
				chkLstFiles.get_Items().Add((object)Path.GetFileName(path), !All);
			}
		}

		private void cmdListAllFiles_Click(object sender, EventArgs e)
		{
			ListFiles(All: true);
		}

		private void cmdSend_Click(object sender, EventArgs e)
		{
			//IL_00f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f6: Invalid comparison between Unknown and I4
			//IL_010a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Unknown result type (might be due to invalid IL or missing references)
			string text = (optMPC.get_Checked() ? ((Control)txtMPC).get_Text() : ((!optJPL.get_Checked()) ? ((Control)txtOther).get_Text() : ((Control)txtJPL).get_Text()));
			string text2 = "Do you want to end an email to " + text + " with names and observations from:";
			for (int i = 0; i < ((ObjectCollection)chkLstFiles.get_Items()).get_Count(); i++)
			{
				if (chkLstFiles.GetItemChecked(i))
				{
					text2 = text2 + "\r\n   * " + ((ObjectCollection)chkLstFiles.get_Items()).get_Item(i).ToString();
				}
			}
			text2 = text2 + "\r\nhaving the following content\r\n\r\n" + ListObservations();
			DisplayData displayData = new DisplayData();
			((ToolStripItem)displayData.cmdCancel).set_Visible(true);
			((Control)displayData.txtBox).set_Text(text2);
			((Control)displayData).set_Width(780);
			((Control)displayData).set_Text("Body of Email report - Confirm Email");
			((TextBoxBase)displayData.txtBox).Select(0, 0);
			if ((int)((Form)displayData).ShowDialog() == 2)
			{
				return;
			}
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet.\r\n\r\nIf the internet cannot be connected, save the report using the 'Save report' button.", "No internet");
				return;
			}
			if (!UploadZippedReport())
			{
				MessageBox.Show("The report has not been Uploaded", "Not uploaded");
			}
			for (int j = 0; j < ((ObjectCollection)chkLstFiles.get_Items()).get_Count(); j++)
			{
				chkLstFiles.SetItemChecked(j, false);
			}
		}

		private bool UploadZippedReport()
		{
			//IL_018c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0192: Invalid comparison between Unknown and I4
			string text = Utilities.AppPath + "\\Asteroid\\Results\\Mpc Files\\OccultationAstrometryReport_" + DateTime.Now.Year + Utilities.ShortMonths[DateTime.Now.Month] + DateTime.Now.Day + ".zip";
			if (File.Exists(text))
			{
				File.Delete(text);
			}
			string[] array = new string[chkLstFiles.get_CheckedItems().get_Count()];
			int num = 0;
			for (int i = 0; i < ((ObjectCollection)chkLstFiles.get_Items()).get_Count(); i++)
			{
				if (chkLstFiles.GetItemChecked(i))
				{
					array[num] = Utilities.AppPath + "\\Asteroid\\Results\\Mpc Files\\" + ((ObjectCollection)chkLstFiles.get_Items()).get_Item(i).ToString();
					num++;
				}
			}
			using (ZipArchive destination = ZipFile.Open(text, ZipArchiveMode.Create))
			{
				string[] array2 = array;
				foreach (string text2 in array2)
				{
					destination.CreateEntryFromFile(text2, new FileInfo(text2).Name);
				}
			}
			FileInfo fileInfo = new FileInfo(text);
			if ((int)MessageBox.Show("The observations have been zipped into the file\r\n\r\n" + text + "\r\n\r\nhaving a length of " + fileInfo.Length + " bytes. Do you want to upload the observations?", "Zip file created", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return false;
			}
			if (!ftp.UploadFTP(text, "incoming/graff2/" + Path.GetFileName(text), Settings.Default.MPC_ftp_uploadAddress))
			{
				return false;
			}
			return EmailUploadNotification(Path.GetFileName(text));
		}

		private bool EmailUploadNotification(string UploadedFile)
		{
			//IL_0063: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_0101: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			string text2 = (optMPC.get_Checked() ? ((Control)txtMPC).get_Text() : ((!optJPL.get_Checked()) ? ((Control)txtOther).get_Text() : ((Control)txtJPL).get_Text()));
			if (text2.IndexOf("@") < 0)
			{
				MessageBox.Show("The selected email address is invalid", "No addresses", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return false;
			}
			if (text2.EndsWith(";"))
			{
				text2 = text2.Replace(";", "");
			}
			string message = "The latest report of astrometry from asteroidal occultations has been uploaded in the file :\r\n" + UploadedFile + "\r\n\r\n" + ((Control)txtMessage).get_Text() + "\r\n\r\n" + ((Control)txtSignatureBlock).get_Text();
			string subject = "Astrometric positions from Occultations";
			text = Emails.Email_AsteroidAstrometry(message, subject, text2);
			if (text.Length > 5)
			{
				MessageBox.Show(text, "Failed email", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else
			{
				MessageBox.Show("Astrometric report has been sent", "Successful email", (MessageBoxButtons)0, (MessageBoxIcon)64);
			}
			if (text.Length > 5)
			{
				return false;
			}
			return true;
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Email asteroids");
		}

		private void cmdListEmail_Click(object sender, EventArgs e)
		{
			//IL_0075: Unknown result type (might be due to invalid IL or missing references)
			string text = ((Control)txtMessage).get_Text() + "\r\n\r\n=====================================\r\nList of names and observations\r\n" + ListObservations();
			DisplayData displayData = new DisplayData();
			displayData.txtBox.set_ScrollBars((ScrollBars)2);
			((ToolStripItem)displayData.cmdCancel).set_Visible(false);
			((ToolStripItem)displayData.cmdOK).set_Visible(false);
			((Control)displayData.txtBox).set_Text(text);
			((Control)displayData).set_Text("Body of Email report");
			((Control)displayData).set_Width(780);
			((TextBoxBase)displayData.txtBox).Select(0, 0);
			((Form)displayData).ShowDialog();
			((Component)(object)displayData).Dispose();
		}

		private string ListObservations()
		{
			string text = "";
			for (int i = 0; i < ((ObjectCollection)chkLstFiles.get_Items()).get_Count(); i++)
			{
				if (chkLstFiles.GetItemChecked(i))
				{
					using StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\Asteroid\\Results\\Mpc Files\\" + ((ObjectCollection)chkLstFiles.get_Items()).get_Item(i).ToString());
					text = text + streamReader.ReadToEnd() + "\r\n\r\n";
				}
			}
			return text;
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
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
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
			//IL_06b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ba: Expected O, but got Unknown
			//IL_0745: Unknown result type (might be due to invalid IL or missing references)
			//IL_074f: Expected O, but got Unknown
			//IL_07da: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e4: Expected O, but got Unknown
			//IL_086f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0879: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(EmailObservations));
			cmdSend = new Button();
			chkLstFiles = new CheckedListBox();
			cmdListUnsent = new Button();
			cmdListAllFiles = new Button();
			label8 = new Label();
			txtMessage = new TextBox();
			optMPC = new RadioButton();
			optJPL = new RadioButton();
			label1 = new Label();
			lblBCC = new Label();
			label2 = new Label();
			radioButton1 = new RadioButton();
			txtOther = new TextBox();
			txtSignatureBlock = new TextBox();
			txtJPL = new TextBox();
			txtMPC = new TextBox();
			label3 = new Label();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdListEmail = new Button();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)cmdSend).set_Location(new Point(729, 259));
			((Control)cmdSend).set_Name("cmdSend");
			((Control)cmdSend).set_Size(new Size(93, 50));
			((Control)cmdSend).set_TabIndex(0);
			((Control)cmdSend).set_Text("ftp upload &&\r\nadvise by email");
			((ButtonBase)cmdSend).set_UseVisualStyleBackColor(true);
			((Control)cmdSend).add_Click((EventHandler)cmdSend_Click);
			chkLstFiles.set_CheckOnClick(true);
			((ListControl)chkLstFiles).set_FormattingEnabled(true);
			((Control)chkLstFiles).set_Location(new Point(12, 51));
			((Control)chkLstFiles).set_Name("chkLstFiles");
			((Control)chkLstFiles).set_Size(new Size(285, 229));
			((Control)chkLstFiles).set_TabIndex(1);
			((Control)cmdListUnsent).set_Location(new Point(30, 291));
			((Control)cmdListUnsent).set_Name("cmdListUnsent");
			((Control)cmdListUnsent).set_Size(new Size(106, 27));
			((Control)cmdListUnsent).set_TabIndex(2);
			((Control)cmdListUnsent).set_Text("List unsent files");
			((ButtonBase)cmdListUnsent).set_UseVisualStyleBackColor(true);
			((Control)cmdListUnsent).add_Click((EventHandler)cmdListUnsent_Click);
			((Control)cmdListAllFiles).set_Location(new Point(168, 291));
			((Control)cmdListAllFiles).set_Name("cmdListAllFiles");
			((Control)cmdListAllFiles).set_Size(new Size(106, 28));
			((Control)cmdListAllFiles).set_TabIndex(3);
			((Control)cmdListAllFiles).set_Text("List all files");
			((ButtonBase)cmdListAllFiles).set_UseVisualStyleBackColor(true);
			((Control)cmdListAllFiles).add_Click((EventHandler)cmdListAllFiles_Click);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(317, 35));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(174, 13));
			((Control)label8).set_TabIndex(35);
			((Control)label8).set_Text("Upload confirmation Email message");
			((Control)txtMessage).set_Location(new Point(317, 51));
			((TextBoxBase)txtMessage).set_Multiline(true);
			((Control)txtMessage).set_Name("txtMessage");
			((Control)txtMessage).set_Size(new Size(520, 126));
			((Control)txtMessage).set_TabIndex(36);
			((Control)txtMessage).set_Text("Please acknowledge receipt.\r\n\r\nThanks.");
			((Control)optMPC).set_AutoSize(true);
			optMPC.set_CheckAlign(ContentAlignment.MiddleRight);
			optMPC.set_Checked(true);
			((Control)optMPC).set_Location(new Point(414, 276));
			((Control)optMPC).set_Name("optMPC");
			((Control)optMPC).set_Size(new Size(48, 17));
			((Control)optMPC).set_TabIndex(39);
			optMPC.set_TabStop(true);
			((Control)optMPC).set_Text("MPC");
			((ButtonBase)optMPC).set_UseVisualStyleBackColor(true);
			((Control)optJPL).set_AutoSize(true);
			optJPL.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optJPL).set_Location(new Point(187, 324));
			((Control)optJPL).set_Name("optJPL");
			((Control)optJPL).set_Size(new Size(87, 17));
			((Control)optJPL).set_TabIndex(40);
			((Control)optJPL).set_Text("JPL-Horizons");
			((ButtonBase)optJPL).set_UseVisualStyleBackColor(true);
			((Control)optJPL).set_Visible(false);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(325, 278));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(50, 13));
			((Control)label1).set_TabIndex(41);
			((Control)label1).set_Text("Send to :");
			((Control)lblBCC).set_AutoSize(true);
			((Control)lblBCC).set_Location(new Point(340, 306));
			((Control)lblBCC).set_Name("lblBCC");
			((Control)lblBCC).set_Size(new Size(134, 13));
			((Control)lblBCC).set_TabIndex(42);
			((Control)lblBCC).set_Text("A BCC copy will be sent to:");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(381, 211));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(81, 13));
			((Control)label2).set_TabIndex(44);
			((Control)label2).set_Text("Signature block");
			((Control)radioButton1).set_AutoSize(true);
			radioButton1.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)radioButton1).set_Location(new Point(509, 324));
			((Control)radioButton1).set_Name("radioButton1");
			((Control)radioButton1).set_Size(new Size(49, 17));
			((Control)radioButton1).set_TabIndex(46);
			((Control)radioButton1).set_Text("other");
			((ButtonBase)radioButton1).set_UseVisualStyleBackColor(true);
			((Control)radioButton1).set_Visible(false);
			((Control)txtOther).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "eMailAddress_other", true, (DataSourceUpdateMode)1));
			((Control)txtOther).set_Location(new Point(569, 322));
			((Control)txtOther).set_Name("txtOther");
			((Control)txtOther).set_Size(new Size(218, 20));
			((Control)txtOther).set_TabIndex(45);
			((Control)txtOther).set_Text(Settings.Default.eMailAddress_other);
			((Control)txtOther).set_Visible(false);
			((Control)txtSignatureBlock).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "eMail_SignatureBlock", true, (DataSourceUpdateMode)1));
			((Control)txtSignatureBlock).set_Location(new Point(472, 192));
			((TextBoxBase)txtSignatureBlock).set_Multiline(true);
			((Control)txtSignatureBlock).set_Name("txtSignatureBlock");
			((Control)txtSignatureBlock).set_Size(new Size(220, 56));
			((Control)txtSignatureBlock).set_TabIndex(43);
			((Control)txtSignatureBlock).set_Text(Settings.Default.eMail_SignatureBlock);
			((Control)txtJPL).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "emailAddress_JPL", true, (DataSourceUpdateMode)1));
			((Control)txtJPL).set_Location(new Point(284, 322));
			((Control)txtJPL).set_Name("txtJPL");
			((Control)txtJPL).set_Size(new Size(220, 20));
			((Control)txtJPL).set_TabIndex(38);
			((Control)txtJPL).set_Text(Settings.Default.emailAddress_JPL);
			((Control)txtJPL).set_Visible(false);
			((Control)txtMPC).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "emailAddress_MPC", true, (DataSourceUpdateMode)1));
			((Control)txtMPC).set_Location(new Point(472, 274));
			((Control)txtMPC).set_Name("txtMPC");
			((Control)txtMPC).set_Size(new Size(220, 20));
			((Control)txtMPC).set_TabIndex(37);
			((Control)txtMPC).set_Text(Settings.Default.emailAddress_MPC);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(12, 35));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(193, 13));
			((Control)label3).set_TabIndex(47);
			((Control)label3).set_Text("Check files to be included in the upload");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(858, 24));
			((Control)menuStrip1).set_TabIndex(48);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdListEmail).set_Location(new Point(729, 192));
			((Control)cmdListEmail).set_Name("cmdListEmail");
			((Control)cmdListEmail).set_Size(new Size(93, 37));
			((Control)cmdListEmail).set_TabIndex(49);
			((Control)cmdListEmail).set_Text("Display content\r\nof report");
			((ButtonBase)cmdListEmail).set_UseVisualStyleBackColor(true);
			((Control)cmdListEmail).add_Click((EventHandler)cmdListEmail_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(858, 337));
			((Control)this).get_Controls().Add((Control)(object)cmdListEmail);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)radioButton1);
			((Control)this).get_Controls().Add((Control)(object)txtOther);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)txtSignatureBlock);
			((Control)this).get_Controls().Add((Control)(object)lblBCC);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)optJPL);
			((Control)this).get_Controls().Add((Control)(object)optMPC);
			((Control)this).get_Controls().Add((Control)(object)txtJPL);
			((Control)this).get_Controls().Add((Control)(object)txtMPC);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)txtMessage);
			((Control)this).get_Controls().Add((Control)(object)cmdListAllFiles);
			((Control)this).get_Controls().Add((Control)(object)cmdListUnsent);
			((Control)this).get_Controls().Add((Control)(object)chkLstFiles);
			((Control)this).get_Controls().Add((Control)(object)cmdSend);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("EmailObservations");
			((Control)this).set_Text("ftp upload of astrometry from Asteroidal Occultations");
			((Form)this).add_Load((EventHandler)EmailObservations_Load);
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
