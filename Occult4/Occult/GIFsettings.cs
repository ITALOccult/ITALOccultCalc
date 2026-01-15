using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class GIFsettings : Form
	{
		private IContainer components;

		private NumericUpDown updnGifDelay;

		private NumericUpDown updnGifRepeats;

		private Label label1;

		private Label label2;

		public GIFsettings()
		{
			InitializeComponent();
		}

		private void GIFsettings_Load(object sender, EventArgs e)
		{
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
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_010d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0117: Expected O, but got Unknown
			//IL_0321: Unknown result type (might be due to invalid IL or missing references)
			//IL_032b: Expected O, but got Unknown
			updnGifRepeats = new NumericUpDown();
			updnGifDelay = new NumericUpDown();
			label1 = new Label();
			label2 = new Label();
			((ISupportInitialize)updnGifRepeats).BeginInit();
			((ISupportInitialize)updnGifDelay).BeginInit();
			((Control)this).SuspendLayout();
			((Control)updnGifRepeats).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GIFRepeats", true, (DataSourceUpdateMode)1));
			((Control)updnGifRepeats).set_Location(new Point(32, 76));
			updnGifRepeats.set_Maximum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			((Control)updnGifRepeats).set_Name("updnGifRepeats");
			((Control)updnGifRepeats).set_Size(new Size(46, 20));
			((Control)updnGifRepeats).set_TabIndex(1);
			((UpDownBase)updnGifRepeats).set_TextAlign((HorizontalAlignment)1);
			updnGifRepeats.set_Value(Settings.Default.GIFRepeats);
			((Control)updnGifDelay).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GIFdelay", true, (DataSourceUpdateMode)1));
			updnGifDelay.set_DecimalPlaces(2);
			updnGifDelay.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)updnGifDelay).set_Location(new Point(12, 12));
			updnGifDelay.set_Maximum(new decimal(new int[4] { 60, 0, 0, 0 }));
			updnGifDelay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnGifDelay).set_Name("updnGifDelay");
			((Control)updnGifDelay).set_Size(new Size(66, 20));
			((Control)updnGifDelay).set_TabIndex(0);
			((UpDownBase)updnGifDelay).set_TextAlign((HorizontalAlignment)1);
			updnGifDelay.set_Value(Settings.Default.GIFdelay);
			((Control)label1).set_Location(new Point(97, 11));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(248, 46));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("Set the time delay (in seconds) for displaying one \r\nimage after the other.  Mimimum allowed value\r\nis 0.01sec. Maximum is 60 secs");
			((Control)label2).set_Location(new Point(97, 74));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(248, 46));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("Set the number of times the animated GIF will\r\nrepeat. For a continuous loop, set the value to 0.\r\nThe maximum allowed value is 1000.");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(362, 129));
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)updnGifRepeats);
			((Control)this).get_Controls().Add((Control)(object)updnGifDelay);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationFilesGIF", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)3);
			((Form)this).set_Location(Settings.Default.LocationFilesGIF);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("GIFsettings");
			((Control)this).set_Text("Set parameters for creating Animated GIF files");
			((Form)this).add_Load((EventHandler)GIFsettings_Load);
			((ISupportInitialize)updnGifRepeats).EndInit();
			((ISupportInitialize)updnGifDelay).EndInit();
			((Control)this).ResumeLayout(false);
		}
	}
}
