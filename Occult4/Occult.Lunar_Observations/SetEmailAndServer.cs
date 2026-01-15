using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class SetEmailAndServer : Form
	{
		private IContainer components;

		private TextBox textBox4;

		private Label label118;

		private Label label10;

		public TextBox FTPPassword;

		private Label label1;

		private Label label2;

		private Button cmdOK;

		private Button cmdCancel;

		public SetEmailAndServer()
		{
			InitializeComponent();
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)1);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)2);
		}

		private void SetEmailAndServer_Load(object sender, EventArgs e)
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
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			//IL_0243: Unknown result type (might be due to invalid IL or missing references)
			//IL_024d: Expected O, but got Unknown
			//IL_0573: Unknown result type (might be due to invalid IL or missing references)
			//IL_057d: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(SetEmailAndServer));
			textBox4 = new TextBox();
			label118 = new Label();
			label10 = new Label();
			FTPPassword = new TextBox();
			label1 = new Label();
			label2 = new Label();
			cmdOK = new Button();
			cmdCancel = new Button();
			((Control)this).SuspendLayout();
			((Control)textBox4).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "EMailServerName", true, (DataSourceUpdateMode)1));
			((Control)textBox4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox4).set_Location(new Point(15, 83));
			((Control)textBox4).set_Name("textBox4");
			((Control)textBox4).set_Size(new Size(221, 20));
			((Control)textBox4).set_TabIndex(35);
			((Control)textBox4).set_Text(Settings.Default.EMailServerName);
			((Control)label118).set_AutoSize(true);
			((Control)label118).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label118).set_Location(new Point(12, 67));
			((Control)label118).set_Name("label118");
			((Control)label118).set_Size(new Size(97, 13));
			((Control)label118).set_TabIndex(34);
			((Control)label118).set_Text("Email Server Name");
			label118.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(378, 67));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(96, 13));
			((Control)label10).set_TabIndex(32);
			((Control)label10).set_Text("Your email address");
			label10.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)FTPPassword).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "FTP_AnonymousPassword", true, (DataSourceUpdateMode)1));
			((Control)FTPPassword).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)FTPPassword).set_Location(new Point(381, 83));
			((Control)FTPPassword).set_Name("FTPPassword");
			((Control)FTPPassword).set_Size(new Size(217, 20));
			((Control)FTPPassword).set_TabIndex(33);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(12, 18));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(555, 30));
			((Control)label1).set_TabIndex(36);
			((Control)label1).set_Text("To submit your observations Occult needs to know your Email address, and the SMTP \r\nserver for your emails.  Please make sure both the fields below have been completed.");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(12, 117));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(329, 91));
			((Control)label2).set_TabIndex(37);
			((Control)label2).set_Text(componentResourceManager.GetString("label2.Text"));
			((Control)cmdOK).set_Location(new Point(408, 149));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(65, 28));
			((Control)cmdOK).set_TabIndex(38);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)cmdCancel).set_Location(new Point(512, 149));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(65, 28));
			((Control)cmdCancel).set_TabIndex(39);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(618, 217));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)textBox4);
			((Control)this).get_Controls().Add((Control)(object)label118);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)FTPPassword);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarSetEmailServer", true, (DataSourceUpdateMode)1));
			((Form)this).set_Location(Settings.Default.LocationLunarSetEmailServer);
			((Control)this).set_Name("SetEmailAndServer");
			((Control)this).set_Text("Set your Email server name and Email address");
			((Form)this).add_Load((EventHandler)SetEmailAndServer_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
