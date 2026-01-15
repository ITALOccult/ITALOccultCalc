using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class EmailObservations : Form
	{
		private ArrayList Addresses = new ArrayList();

		internal bool IsGraze;

		private IContainer components;

		private ListBox lstAddresses;

		private Button cmdOK;

		private Button cmdCancel;

		internal TextBox txtAddress;

		private Label label1;

		private Label label2;

		private Label lblGrazeMessage;

		internal TextBox txtMyEmail;

		private Label label3;

		private Label label4;

		private ToolTip toolTip1;

		private Button cmdHelp;

		public EmailObservations()
		{
			InitializeComponent();
		}

		private void EmailObservations_Load(object sender, EventArgs e)
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
			string path = Utilities.AppPath + "\\Resource Files\\addresses.txt";
			int num = 0;
			Addresses.Clear();
			if (File.Exists(path))
			{
				using StreamReader streamReader = new StreamReader(path);
				while (!streamReader.EndOfStream)
				{
					string text = streamReader.ReadLine();
					string text2 = text.ToUpper().Substring(0, 4);
					if (!(text2 == "<LIG") && !(text2 == "<LIG") && !(text2 == "<DOU"))
					{
						int num2 = text.IndexOf(">");
						if ((num2 > 1) & (num2 < text.Length - 1))
						{
							lstAddresses.get_Items().Add((object)text.Substring(1, num2 - 1));
							Addresses.Add(text.Substring(num2 + 1));
						}
					}
				}
			}
			for (num = 0; num < lstAddresses.get_Items().get_Count(); num++)
			{
				if (lstAddresses.get_Items().get_Item(num).ToString() == Settings.Default.LastLunarReportAddress)
				{
					((ListControl)lstAddresses).set_SelectedIndex(num);
					break;
				}
			}
			for (num = 0; num < LunarObservations.OccMain.Events.Count; num++)
			{
				if (LunarObservations.OccMain.Events[num].GrazeFlag == "G")
				{
					IsGraze = true;
					((Control)lblGrazeMessage).set_Visible(true);
					int num3 = lstAddresses.FindString("grazes");
					if (num3 < 0)
					{
						num3 = lstAddresses.FindString("Grazes");
					}
					if (num3 < 0)
					{
						num3 = lstAddresses.FindString("GRAZES");
					}
					if (num3 >= 0)
					{
						((ListControl)lstAddresses).set_SelectedIndex(num3);
					}
					break;
				}
			}
			((Control)txtMyEmail).set_Text(Settings.Default.FTP_AnonymousPassword);
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			if (((Control)txtAddress).get_Text().Length < 10)
			{
				((Form)this).set_DialogResult((DialogResult)2);
			}
			else
			{
				((Form)this).set_DialogResult((DialogResult)1);
			}
			string text = lstAddresses.get_Items().get_Item(((ListControl)lstAddresses).get_SelectedIndex()).ToString();
			if (!text.ToUpper().Contains("GRAZES"))
			{
				Settings.Default.LastLunarReportAddress = text;
			}
			((Form)this).Close();
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)2);
			((Form)this).Close();
		}

		private void lstAddresses_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstAddresses).get_SelectedIndex() > -1)
			{
				((Control)txtAddress).set_Text(Addresses[((ListControl)lstAddresses).get_SelectedIndex()]!.ToString());
			}
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Reporting Lunar observations 2");
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
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_070f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0719: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(EmailObservations));
			lstAddresses = new ListBox();
			txtAddress = new TextBox();
			cmdOK = new Button();
			cmdCancel = new Button();
			label1 = new Label();
			label2 = new Label();
			lblGrazeMessage = new Label();
			txtMyEmail = new TextBox();
			label3 = new Label();
			label4 = new Label();
			toolTip1 = new ToolTip(components);
			cmdHelp = new Button();
			((Control)this).SuspendLayout();
			((ListControl)lstAddresses).set_FormattingEnabled(true);
			((Control)lstAddresses).set_Location(new Point(12, 162));
			((Control)lstAddresses).set_Name("lstAddresses");
			((Control)lstAddresses).set_Size(new Size(194, 199));
			((Control)lstAddresses).set_TabIndex(0);
			lstAddresses.add_SelectedIndexChanged((EventHandler)lstAddresses_SelectedIndexChanged);
			((Control)txtAddress).set_Location(new Point(230, 162));
			((Control)txtAddress).set_Name("txtAddress");
			((TextBoxBase)txtAddress).set_ReadOnly(true);
			((Control)txtAddress).set_Size(new Size(301, 20));
			((Control)txtAddress).set_TabIndex(1);
			((Control)cmdOK).set_Location(new Point(282, 322));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(70, 37));
			((Control)cmdOK).set_TabIndex(2);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)cmdCancel).set_Location(new Point(397, 322));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(70, 37));
			((Control)cmdCancel).set_TabIndex(3);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(9, 146));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(195, 13));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("Select an address from the following list.");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(230, 146));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(208, 13));
			((Control)label2).set_TabIndex(5);
			((Control)label2).set_Text("Email address - the report will be sent here.");
			((Control)lblGrazeMessage).set_AutoSize(true);
			((Control)lblGrazeMessage).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblGrazeMessage).set_Location(new Point(230, 193));
			((Control)lblGrazeMessage).set_Name("lblGrazeMessage");
			((Control)lblGrazeMessage).set_Size(new Size(379, 65));
			((Control)lblGrazeMessage).set_TabIndex(6);
			((Control)lblGrazeMessage).set_Text(componentResourceManager.GetString("lblGrazeMessage.Text"));
			((Control)lblGrazeMessage).set_Visible(false);
			((Control)txtMyEmail).set_Location(new Point(230, 296));
			((Control)txtMyEmail).set_Name("txtMyEmail");
			((TextBoxBase)txtMyEmail).set_ReadOnly(true);
			((Control)txtMyEmail).set_Size(new Size(301, 20));
			((Control)txtMyEmail).set_TabIndex(7);
			toolTip1.SetToolTip((Control)(object)txtMyEmail, "Your email address is set in Maintenance, User Settings - in the internet group");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(230, 283));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(250, 13));
			((Control)label3).set_TabIndex(8);
			((Control)label3).set_Text("'My' Email address - the report will be copied to here");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(12, 9));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(536, 105));
			((Control)label4).set_TabIndex(9);
			((Control)label4).set_Text(componentResourceManager.GetString("label4.Text"));
			toolTip1.set_AutomaticDelay(200);
			toolTip1.set_AutoPopDelay(6000);
			toolTip1.set_InitialDelay(200);
			toolTip1.set_ReshowDelay(40);
			((Control)cmdHelp).set_Location(new Point(513, 326));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(49, 29));
			((Control)cmdHelp).set_TabIndex(10);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(612, 378));
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)txtMyEmail);
			((Control)this).get_Controls().Add((Control)(object)lblGrazeMessage);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)txtAddress);
			((Control)this).get_Controls().Add((Control)(object)lstAddresses);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarEmail", true, (DataSourceUpdateMode)1));
			((Form)this).set_Location(Settings.Default.LocationLunarEmail);
			((Control)this).set_Name("EmailObservations");
			((Control)this).set_Text("Select a reporting address to Email observations");
			((Form)this).add_Load((EventHandler)EmailObservations_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
