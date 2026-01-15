using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class GrazeMessage : Form
	{
		internal ArrayList Addresses = new ArrayList();

		private IContainer components;

		internal TextBox txtMessage;

		private Button cmdOK;

		private Button cmdCancel;

		private Label label1;

		internal CheckBox chkReport;

		internal CheckBox chkReductions;

		internal CheckBox chkProfile;

		private Button cmdAll;

		private Button cmdNone;

		private Button cmdCancelKeepText;

		private Label label2;

		private Label label3;

		internal TextBox txtMe;

		private Label label4;

		private Label label5;

		internal CheckedListBox lstObservers;

		private Button cmdHelp;

		public GrazeMessage()
		{
			InitializeComponent();
		}

		private void GrazeMessage_Load(object sender, EventArgs e)
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
			((Control)txtMessage).set_Text(Emails.GrazeMessageText);
			for (int i = 0; i < LunarObservations.OccMain.Observers.Count; i++)
			{
				if (LunarObservations.OccMain.Observers[i].ObserverEmail.Contains("@"))
				{
					Addresses.Add(LunarObservations.OccMain.Observers[i].ObserverEmail);
					((ObjectCollection)lstObservers.get_Items()).Add((object)LunarObservations.OccMain.Observers[i].ObserverName);
				}
			}
			((Control)txtMe).set_Text(Settings.Default.FTP_AnonymousPassword);
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			Emails.GrazeMessageText = ((Control)txtMessage).get_Text();
			((Form)this).Close();
			((Form)this).set_DialogResult((DialogResult)1);
		}

		private void cmdCancelKeepText_Click(object sender, EventArgs e)
		{
			Emails.GrazeMessageText = ((Control)txtMessage).get_Text();
			((Form)this).Close();
			((Form)this).set_DialogResult((DialogResult)2);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Form)this).set_DialogResult((DialogResult)2);
		}

		private void cmdAll_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)lstObservers.get_Items()).get_Count(); i++)
			{
				lstObservers.SetItemChecked(i, true);
			}
		}

		private void cmdNone_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)lstObservers.get_Items()).get_Count(); i++)
			{
				lstObservers.SetItemChecked(i, false);
			}
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Graze organiser messages");
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
			//IL_09bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_09c7: Expected O, but got Unknown
			txtMessage = new TextBox();
			lstObservers = new CheckedListBox();
			cmdOK = new Button();
			cmdCancel = new Button();
			label1 = new Label();
			chkReport = new CheckBox();
			chkReductions = new CheckBox();
			chkProfile = new CheckBox();
			cmdAll = new Button();
			cmdNone = new Button();
			cmdCancelKeepText = new Button();
			label2 = new Label();
			txtMe = new TextBox();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			cmdHelp = new Button();
			((Control)this).SuspendLayout();
			((Control)txtMessage).set_Location(new Point(19, 71));
			((TextBoxBase)txtMessage).set_Multiline(true);
			((Control)txtMessage).set_Name("txtMessage");
			txtMessage.set_ScrollBars((ScrollBars)2);
			((Control)txtMessage).set_Size(new Size(390, 274));
			((Control)txtMessage).set_TabIndex(0);
			lstObservers.set_CheckOnClick(true);
			((ListControl)lstObservers).set_FormattingEnabled(true);
			((Control)lstObservers).set_Location(new Point(428, 71));
			((Control)lstObservers).set_Name("lstObservers");
			((Control)lstObservers).set_Size(new Size(190, 274));
			((Control)lstObservers).set_TabIndex(1);
			((Control)cmdOK).set_Location(new Point(656, 181));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(81, 35));
			((Control)cmdOK).set_TabIndex(2);
			((Control)cmdOK).set_Text("Send message");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)cmdCancel).set_Location(new Point(656, 289));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(81, 35));
			((Control)cmdCancel).set_TabIndex(3);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(445, 55));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(156, 13));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("Observers with Email addresses");
			((Control)chkReport).set_AutoSize(true);
			((Control)chkReport).set_Location(new Point(657, 74));
			((Control)chkReport).set_Name("chkReport");
			((Control)chkReport).set_Size(new Size(58, 17));
			((Control)chkReport).set_TabIndex(5);
			((Control)chkReport).set_Text("Report");
			((ButtonBase)chkReport).set_UseVisualStyleBackColor(true);
			((Control)chkReductions).set_AutoSize(true);
			((Control)chkReductions).set_Location(new Point(657, 96));
			((Control)chkReductions).set_Name("chkReductions");
			((Control)chkReductions).set_Size(new Size(80, 17));
			((Control)chkReductions).set_TabIndex(6);
			((Control)chkReductions).set_Text("Reductions");
			((ButtonBase)chkReductions).set_UseVisualStyleBackColor(true);
			((Control)chkProfile).set_AutoSize(true);
			((Control)chkProfile).set_Location(new Point(657, 119));
			((Control)chkProfile).set_Name("chkProfile");
			((Control)chkProfile).set_Size(new Size(55, 17));
			((Control)chkProfile).set_TabIndex(7);
			((Control)chkProfile).set_Text("Profile");
			((ButtonBase)chkProfile).set_UseVisualStyleBackColor(true);
			((Control)cmdAll).set_Location(new Point(434, 361));
			((Control)cmdAll).set_Name("cmdAll");
			((Control)cmdAll).set_Size(new Size(74, 22));
			((Control)cmdAll).set_TabIndex(8);
			((Control)cmdAll).set_Text("Select all");
			((ButtonBase)cmdAll).set_UseVisualStyleBackColor(true);
			((Control)cmdAll).add_Click((EventHandler)cmdAll_Click);
			((Control)cmdNone).set_Location(new Point(532, 361));
			((Control)cmdNone).set_Name("cmdNone");
			((Control)cmdNone).set_Size(new Size(74, 22));
			((Control)cmdNone).set_TabIndex(9);
			((Control)cmdNone).set_Text("Clear all");
			((ButtonBase)cmdNone).set_UseVisualStyleBackColor(true);
			((Control)cmdNone).add_Click((EventHandler)cmdNone_Click);
			((Control)cmdCancelKeepText).set_Location(new Point(656, 235));
			((Control)cmdCancelKeepText).set_Name("cmdCancelKeepText");
			((Control)cmdCancelKeepText).set_Size(new Size(81, 35));
			((Control)cmdCancelKeepText).set_TabIndex(10);
			((Control)cmdCancelKeepText).set_Text("Cancel, but keep text");
			((ButtonBase)cmdCancelKeepText).set_UseVisualStyleBackColor(true);
			((Control)cmdCancelKeepText).add_Click((EventHandler)cmdCancelKeepText_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(189, 55));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(50, 13));
			((Control)label2).set_TabIndex(11);
			((Control)label2).set_Text("Message");
			((Control)txtMe).set_Location(new Point(124, 363));
			((Control)txtMe).set_Name("txtMe");
			((TextBoxBase)txtMe).set_ReadOnly(true);
			((Control)txtMe).set_Size(new Size(262, 20));
			((Control)txtMe).set_TabIndex(12);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(29, 367));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(89, 13));
			((Control)label3).set_TabIndex(13);
			((Control)label3).set_Text("My Email address");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(16, 9));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(701, 26));
			((Control)label4).set_TabIndex(14);
			((Control)label4).set_Text("To send a message: Write your message in the Message box, select the recipients from the list of Observers, and indicate \r\nwhich attachments (if any) are to be included.");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(652, 55));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(66, 13));
			((Control)label5).set_TabIndex(15);
			((Control)label5).set_Text("Attachments");
			((Control)cmdHelp).set_Location(new Point(668, 343));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(56, 25));
			((Control)cmdHelp).set_TabIndex(16);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(767, 403));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)txtMe);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)cmdCancelKeepText);
			((Control)this).get_Controls().Add((Control)(object)cmdNone);
			((Control)this).get_Controls().Add((Control)(object)cmdAll);
			((Control)this).get_Controls().Add((Control)(object)chkProfile);
			((Control)this).get_Controls().Add((Control)(object)chkReductions);
			((Control)this).get_Controls().Add((Control)(object)chkReport);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)lstObservers);
			((Control)this).get_Controls().Add((Control)(object)txtMessage);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarGrazeMessage", true, (DataSourceUpdateMode)1));
			((Form)this).set_Location(Settings.Default.LocationLunarGrazeMessage);
			((Control)this).set_Name("GrazeMessage");
			((Control)this).set_Text("Send a message to one or more observers");
			((Form)this).add_Load((EventHandler)GrazeMessage_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
