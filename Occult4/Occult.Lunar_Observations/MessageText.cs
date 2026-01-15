using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class MessageText : Form
	{
		private IContainer components;

		private Label label1;

		private Button cmdOK;

		private Button cmdCancel;

		internal TextBox txtMessage;

		private Label label2;

		private TextBox txtAddresses;

		public MessageText(string AddressList)
		{
			InitializeComponent();
			((Control)txtAddresses).set_Text(AddressList);
			((Control)txtMessage).set_Text(Emails.MessageText);
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			Emails.MessageText = ((Control)txtMessage).get_Text();
			((Form)this).set_DialogResult((DialogResult)1);
			((Form)this).Close();
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			Emails.MessageText = ((Control)txtMessage).get_Text();
			((Form)this).set_DialogResult((DialogResult)2);
			((Form)this).Close();
		}

		private void MessageText_Load(object sender, EventArgs e)
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
			//IL_03c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d1: Expected O, but got Unknown
			txtMessage = new TextBox();
			label1 = new Label();
			cmdOK = new Button();
			cmdCancel = new Button();
			label2 = new Label();
			txtAddresses = new TextBox();
			((Control)this).SuspendLayout();
			((Control)txtMessage).set_Location(new Point(12, 31));
			((TextBoxBase)txtMessage).set_Multiline(true);
			((Control)txtMessage).set_Name("txtMessage");
			txtMessage.set_ScrollBars((ScrollBars)2);
			((Control)txtMessage).set_Size(new Size(635, 295));
			((Control)txtMessage).set_TabIndex(0);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(12, 9));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(551, 15));
			((Control)label1).set_TabIndex(1);
			((Control)label1).set_Text("Write here any message to the representative. It will be included in the Email as a 'special message'.\r\n");
			((Control)cmdOK).set_Location(new Point(434, 361));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(88, 23));
			((Control)cmdOK).set_TabIndex(2);
			((Control)cmdOK).set_Text("Send Email");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)cmdCancel).set_Location(new Point(559, 361));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(88, 23));
			((Control)cmdCancel).set_TabIndex(3);
			((Control)cmdCancel).set_Text("Cancel Email");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(12, 330));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(72, 13));
			((Control)label2).set_TabIndex(4);
			((Control)label2).set_Text("Address list");
			((Control)txtAddresses).set_Location(new Point(12, 345));
			((TextBoxBase)txtAddresses).set_Multiline(true);
			((Control)txtAddresses).set_Name("txtAddresses");
			((TextBoxBase)txtAddresses).set_ReadOnly(true);
			txtAddresses.set_ScrollBars((ScrollBars)2);
			((Control)txtAddresses).set_Size(new Size(385, 62));
			((Control)txtAddresses).set_TabIndex(6);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(660, 413));
			((Form)this).set_ControlBox(false);
			((Control)this).get_Controls().Add((Control)(object)txtAddresses);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtMessage);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarMessageText", true, (DataSourceUpdateMode)1));
			((Form)this).set_FormBorderStyle((FormBorderStyle)5);
			((Form)this).set_Location(Settings.Default.LocationLunarMessageText);
			((Control)this).set_Name("MessageText");
			((Control)this).set_Text("Message text");
			((Form)this).add_Load((EventHandler)MessageText_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
