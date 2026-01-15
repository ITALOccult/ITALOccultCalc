using System;
using System.Collections;
using System.Diagnostics;
using System.IO;
using System.IO.Compression;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	internal class GoogleEarth
	{
		public static string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private static StreamWriter GoogleEarthKMLFile;

		private static StreamWriter GoogleMapFile;

		private static StreamWriter MIF_file;

		private static StreamWriter CMX_file;

		private static StreamWriter GPX_file;

		private static StreamWriter PLT_file;

		private static StreamWriter GEN_file;

		private static StreamWriter DeLorme_file;

		public static readonly string[] Colours = new string[39]
		{
			"White", "Red", "Green", "Blue", "Brown", "Chocolate", "Crimson", "Cyan", "DarkBlue", "DarkRed",
			"DarkViolet", "Gold", "GoldenRod", "Maroon", "OrangeRed", "Sienna", "Yellow", "Black", "Gray", "White_Wide",
			"Red_Wide", "Green_Wide", "Blue_Wide", "Brown_Wide", "Chocolate_Wide", "Crimson_Wide", "Cyan_Wide", "DarkBlue_Wide", "DarkRed_Wide", "DarkViolet_Wide",
			"Gold_Wide", "GoldenRod_Wide", "Maroon_Wide", "OrangeRed_Wide", "Sienna_Wide", "Yellow_Wide", "Black_Wide", "Gray_Wide", "Magenta"
		};

		private static bool GoogleMapFirstOutput = true;

		private static bool PolyPointsExist = false;

		private static double LastLongitude = 0.0;

		private static double LastLatitude = 0.0;

		private static int PolypointCounter = 0;

		private static ArrayList MIFpoints;

		private static ArrayList CMXpoints;

		private static ArrayList GPXpoints;

		private static ArrayList PLTpoints;

		private static ArrayList GENpoints;

		private static ArrayList DeLormepoints;

		public static bool Create_New_GoogleEarthKMZ_File(string OutFile_withPath, string Label, bool AutoOpenFile, out string CreatedFile)
		{
			//IL_000f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0071: Unknown result type (might be due to invalid IL or missing references)
			//IL_0077: Invalid comparison between Unknown and I4
			string text = OutFile_withPath;
			CreatedFile = "";
			if (!AutoOpenFile)
			{
				SaveFileDialog val = new SaveFileDialog();
				((FileDialog)val).set_CheckPathExists(true);
				((FileDialog)val).set_Title(OutFile_withPath);
				val.set_OverwritePrompt(true);
				try
				{
					((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.FilePathGoogleEarthKML));
				}
				catch
				{
				}
				((FileDialog)val).set_FileName(OutFile_withPath + ".kml");
				((FileDialog)val).set_Filter("kml files (*.kml)|*.kml|All files (*.*)|*.*");
				((FileDialog)val).set_FilterIndex(0);
				if ((int)((CommonDialog)val).ShowDialog() != 1)
				{
					return false;
				}
				text = ((FileDialog)val).get_FileName();
				Settings.Default.FilePathGoogleEarthKML = ((FileDialog)val).get_FileName();
			}
			CreatedFile = text;
			try
			{
				GoogleEarthKMLFile.Close();
			}
			catch
			{
			}
			string directoryName = Path.GetDirectoryName(text);
			if (!Directory.Exists(directoryName))
			{
				Directory.CreateDirectory(directoryName);
			}
			GoogleEarthKMLFile = new StreamWriter(text, append: false);
			string text2 = Settings.Default.GoogleEarthLineWidth.ToString();
			string text3 = ((int)(3.0 * double.Parse(text2))).ToString();
			GoogleEarthKMLFile.WriteLine("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
			GoogleEarthKMLFile.WriteLine("<kml xmlns=\"http://www.opengis.net/kml/2.2\">");
			GoogleEarthKMLFile.WriteLine("<Document>");
			GoogleEarthKMLFile.WriteLine("  <description>" + Label + "</description>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"White\"><LineStyle><color>ffffffff</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Red\"><LineStyle><color>ff0000ff</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Green\"><LineStyle><color>ff00ff00</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Blue\"><LineStyle><color>ffff0000</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Brown\"><LineStyle><color>ff2A2AA5</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Chocolate\"><LineStyle><color>ff1E69D2</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Crimson\"><LineStyle><color>ff3C14DC</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Cyan\"><LineStyle><color>ffFFFF00</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"DarkBlue\"><LineStyle><color>ff8B0000</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"DarkRed\"><LineStyle><color>ff00008B</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"DarkViolet\"><LineStyle><color>ffD30094</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Gold\"><LineStyle><color>ff00D7FF</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"GoldenRod\"><LineStyle><color>ff20A5DA</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Maroon\"><LineStyle><color>ff008888</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"OrangeRed\"><LineStyle><color>ff0045FF</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Sienna\"><LineStyle><color>ff2D52A0</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Yellow\"><LineStyle><color>ff00FFFF</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Black\"><LineStyle><color>ff000000</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Gray\"><LineStyle><color>ff888888</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"White_Wide\"><LineStyle><color>ffffffff</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Red_Wide\"><LineStyle><color>ff0000ff</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Green_Wide\"><LineStyle><color>ff00ff00</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Blue_Wide\"><LineStyle><color>ffff0000</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Brown_Wide\"><LineStyle><color>ff2A2AA5</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Chocolate_Wide\"><LineStyle><color>ff1E69D2</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Crimson_Wide\"><LineStyle><color>ff3C14DC</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Cyan_Wide\"><LineStyle><color>ffFFFF00</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"DarkBlue_Wide\"><LineStyle><color>ff8B0000</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"DarkRed_Wide\"><LineStyle><color>ff00008B</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"DarkViolet_Wide\"><LineStyle><color>ffD30094</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Gold_Wide\"><LineStyle><color>ff00D7FF</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"GoldenRod_Wide\"><LineStyle><color>ff20A5DA</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Maroon_Wide\"><LineStyle><color>ff008888</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"OrangeRed_Wide\"><LineStyle><color>ff0045FF</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Sienna_Wide\"><LineStyle><color>ff2D52A0</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Yellow_Wide\"><LineStyle><color>ff00FFFF</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Black_Wide\"><LineStyle><color>ff000000</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Gray_Wide\"><LineStyle><color>ff888888</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Magenta\"><LineStyle><color>ffff00ff</color><width>" + text3 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"cp\"><LineStyle><color>ff00ff00</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"path\"><LineStyle><color>ff0000ff</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"1sigma\"><LineStyle><color>FFFF0000</color><width>" + text2 + "</width></LineStyle></Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Time\">");
			GoogleEarthKMLFile.WriteLine("    <IconStyle>");
			GoogleEarthKMLFile.WriteLine("      <Icon>");
			GoogleEarthKMLFile.WriteLine("        <href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>");
			GoogleEarthKMLFile.WriteLine("      </Icon>");
			GoogleEarthKMLFile.WriteLine("    </IconStyle>");
			GoogleEarthKMLFile.WriteLine("    <LabelStyle><scale>0.0</scale></LabelStyle>");
			GoogleEarthKMLFile.WriteLine("  </Style>");
			GoogleEarthKMLFile.WriteLine("  <Style id=\"Height\">");
			GoogleEarthKMLFile.WriteLine("    <IconStyle>");
			GoogleEarthKMLFile.WriteLine("      <Icon>");
			GoogleEarthKMLFile.WriteLine("        <href>http://maps.google.com/mapfiles/kml/shapes/open-diamond.png</href>");
			GoogleEarthKMLFile.WriteLine("      </Icon>");
			GoogleEarthKMLFile.WriteLine("    </IconStyle>");
			GoogleEarthKMLFile.WriteLine("    <LabelStyle><scale>0.8</scale></LabelStyle>");
			GoogleEarthKMLFile.WriteLine("  </Style>");
			return true;
		}

		public static void Write_Tags_For_New_GoogleEarthKMZ_Path(int color)
		{
			GoogleEarthKMLFile.WriteLine("  <Placemark>");
			GoogleEarthKMLFile.WriteLine("    <styleUrl>#" + Colours[color] + "</styleUrl>");
			GoogleEarthKMLFile.WriteLine("    <LineString>");
			GoogleEarthKMLFile.WriteLine("      <extrude>1</extrude>");
			GoogleEarthKMLFile.WriteLine("      <tessellate>1</tessellate>");
			GoogleEarthKMLFile.WriteLine("      <altitudeMode>clampToGround</altitudeMode>");
			GoogleEarthKMLFile.WriteLine("      <coordinates>");
		}

		public static void Write_TagsAndLabel_For_New_GoogleEarthKMZ_Path(int color, string Label)
		{
			GoogleEarthKMLFile.WriteLine("  <Placemark>");
			GoogleEarthKMLFile.WriteLine("    <styleUrl>#" + Colours[color] + "</styleUrl>");
			GoogleEarthKMLFile.WriteLine("    <name>" + Label + "</name>");
			GoogleEarthKMLFile.WriteLine("    <LineString>");
			GoogleEarthKMLFile.WriteLine("      <extrude>1</extrude>");
			GoogleEarthKMLFile.WriteLine("      <tessellate>1</tessellate>");
			GoogleEarthKMLFile.WriteLine("      <altitudeMode>clampToGround</altitudeMode>");
			GoogleEarthKMLFile.WriteLine("      <coordinates>");
		}

		public static void Write_TagsAndLabel_For_New_GoogleEarthKMZ_Path(int color, string Label, bool UseWideLines)
		{
			GoogleEarthKMLFile.WriteLine("  <Placemark>");
			if (UseWideLines)
			{
				GoogleEarthKMLFile.WriteLine("    <styleUrl>#" + Colours[color + 19] + "</styleUrl>");
			}
			else
			{
				GoogleEarthKMLFile.WriteLine("    <styleUrl>#" + Colours[color] + "</styleUrl>");
			}
			GoogleEarthKMLFile.WriteLine("    <name>" + Label + "</name>");
			GoogleEarthKMLFile.WriteLine("    <LineString>");
			GoogleEarthKMLFile.WriteLine("      <extrude>1</extrude>");
			GoogleEarthKMLFile.WriteLine("      <tessellate>1</tessellate>");
			GoogleEarthKMLFile.WriteLine("      <altitudeMode>clampToGround</altitudeMode>");
			GoogleEarthKMLFile.WriteLine("      <coordinates>");
		}

		public static void Write_Path_Coordinate_GoogleEarthKMZ(double Longitude, double Latitude, double Altitude)
		{
			if (Longitude > 180.0)
			{
				Longitude -= 360.0;
			}
			if (Longitude < -180.0)
			{
				Longitude += 360.0;
			}
			GoogleEarthKMLFile.WriteLine("        " + string.Format("{0,1:F5},{1,1:F5},{2,1:F0}", Longitude, Latitude, Altitude));
		}

		public static void Write_Tags_At_End_of_Path_GoogleEarthKMZ()
		{
			GoogleEarthKMLFile.WriteLine("      </coordinates>");
			GoogleEarthKMLFile.WriteLine("    </LineString>");
			GoogleEarthKMLFile.WriteLine("  </Placemark>");
		}

		public static void OpenFolder(string FolderName)
		{
			GoogleEarthKMLFile.WriteLine("  <Folder>");
			GoogleEarthKMLFile.WriteLine("    <name>");
			GoogleEarthKMLFile.WriteLine("      " + FolderName);
			GoogleEarthKMLFile.WriteLine("    </name>");
		}

		public static void Write_PlaceMark_PinPlusName_GoogleEarthKML(string Name, double Longitude, double Latitude)
		{
			GoogleEarthKMLFile.WriteLine("  <Placemark>");
			GoogleEarthKMLFile.WriteLine("    <name>" + Name.Trim().Replace("&", "+") + "</name>");
			GoogleEarthKMLFile.WriteLine("    <description>" + Name.Trim().Replace("&", "+") + "</description>");
			GoogleEarthKMLFile.WriteLine("    <Point>");
			GoogleEarthKMLFile.WriteLine("       <coordinates>" + string.Format("{0,1:F5},{1,1:F5}", Longitude, Latitude) + "</coordinates>");
			GoogleEarthKMLFile.WriteLine("    </Point>");
			GoogleEarthKMLFile.WriteLine("  </Placemark>");
		}

		public static void Write_PlaceMark_PinPlusName_GoogleEarthKML(string Name, double Longitude, double Latitude, int PinColor_0Yel_1Blu_2Grn_3LtBlu_4Pink_5Purp_6Red_7White)
		{
			string[] array = new string[8] { "ylw", "blue", "grn", "ltblu", "pink", "purple", "red", "wht" };
			GoogleEarthKMLFile.WriteLine("  <Placemark>");
			GoogleEarthKMLFile.WriteLine("    <Style>");
			GoogleEarthKMLFile.WriteLine("      <IconStyle>");
			GoogleEarthKMLFile.WriteLine("        <Icon>");
			GoogleEarthKMLFile.WriteLine("          <href>http://maps.google.com/mapfiles/kml/pushpin/" + array[PinColor_0Yel_1Blu_2Grn_3LtBlu_4Pink_5Purp_6Red_7White] + "-pushpin.png</href>");
			GoogleEarthKMLFile.WriteLine("        </Icon>");
			GoogleEarthKMLFile.WriteLine("      </IconStyle>");
			GoogleEarthKMLFile.WriteLine("     </Style>");
			GoogleEarthKMLFile.WriteLine("    <name>" + Name.Trim().Replace("&", "+") + "</name>");
			GoogleEarthKMLFile.WriteLine("    <description>" + Name.Trim().Replace("&", "+") + "</description>");
			GoogleEarthKMLFile.WriteLine("    <Point>");
			GoogleEarthKMLFile.WriteLine("       <coordinates>" + string.Format("{0,1:F5},{1,1:F5}", Longitude, Latitude) + "</coordinates>");
			GoogleEarthKMLFile.WriteLine("    </Point>");
			GoogleEarthKMLFile.WriteLine("  </Placemark>");
		}

		public static void Write_PlaceMark_BullseyePlusDescriptionBox_GoogleEarthKML(string Name, string Description, double Longitude, double Latitude)
		{
			GoogleEarthKMLFile.WriteLine("   <Placemark>");
			GoogleEarthKMLFile.WriteLine("     <Style>");
			GoogleEarthKMLFile.WriteLine("       <IconStyle>");
			GoogleEarthKMLFile.WriteLine("         <Icon>");
			GoogleEarthKMLFile.WriteLine("           <href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>");
			GoogleEarthKMLFile.WriteLine("         </Icon>");
			GoogleEarthKMLFile.WriteLine("       </IconStyle>");
			GoogleEarthKMLFile.WriteLine("       <LabelStyle>");
			GoogleEarthKMLFile.WriteLine("         <scale>0</scale>");
			GoogleEarthKMLFile.WriteLine("       </LabelStyle>");
			GoogleEarthKMLFile.WriteLine("       <BalloonStyle>");
			GoogleEarthKMLFile.WriteLine("         <bgColor>fffffdd0</bgColor>");
			GoogleEarthKMLFile.WriteLine("       </BalloonStyle>");
			GoogleEarthKMLFile.WriteLine("     </Style>");
			GoogleEarthKMLFile.WriteLine("     <name>" + Name.Trim().Replace("&", "+") + "</name>");
			GoogleEarthKMLFile.WriteLine("     <snippet></snippet>");
			GoogleEarthKMLFile.WriteLine("     <description>");
			GoogleEarthKMLFile.WriteLine("       " + Description);
			GoogleEarthKMLFile.WriteLine("     </description>");
			GoogleEarthKMLFile.WriteLine("     <Point>");
			GoogleEarthKMLFile.WriteLine("        <coordinates>" + string.Format("{0,1:F5},{1,1:F5}", Longitude, Latitude) + "</coordinates>");
			GoogleEarthKMLFile.WriteLine("     </Point>");
			GoogleEarthKMLFile.WriteLine("   </Placemark>");
		}

		public static void Write_PlaceMark_DiamondPlusHeight_GoogleEarthKML(string Height, double Longitude, double Latitude)
		{
			GoogleEarthKMLFile.WriteLine("  <Placemark>");
			GoogleEarthKMLFile.WriteLine("    <name>" + Height.Trim().Replace("&", "+") + "</name>");
			GoogleEarthKMLFile.WriteLine("    <styleUrl>#Height</styleUrl>");
			GoogleEarthKMLFile.WriteLine("    <Point>");
			GoogleEarthKMLFile.WriteLine("       <coordinates>" + string.Format("{0,1:F5},{1,1:F5}", Longitude, Latitude) + "</coordinates>");
			GoogleEarthKMLFile.WriteLine("    </Point>");
			GoogleEarthKMLFile.WriteLine("  </Placemark>");
		}

		public static void CloseFolder()
		{
			GoogleEarthKMLFile.WriteLine("  </Folder>");
		}

		public static void Set_OpenLocation(double Longitude, double Latitude)
		{
			GoogleEarthKMLFile.WriteLine("  <LookAt>");
			GoogleEarthKMLFile.WriteLine("    <longitude>" + string.Format("{0,1:F5}", Longitude) + "</longitude>");
			GoogleEarthKMLFile.WriteLine("    <latitude>" + string.Format("{0,1:F5}", Latitude) + "</latitude>");
			GoogleEarthKMLFile.WriteLine("    <altitude>0</altitude>");
			GoogleEarthKMLFile.WriteLine("    <heading>0</heading>");
			GoogleEarthKMLFile.WriteLine("    <tilt>0</tilt>");
			GoogleEarthKMLFile.WriteLine("    <range>1000000</range>");
			GoogleEarthKMLFile.WriteLine("  </LookAt>");
		}

		public static string Close_GoogleEarthKMZ_File(string CreatedFile, bool ConvertToKMZ)
		{
			GoogleEarthKMLFile.WriteLine("</Document>");
			GoogleEarthKMLFile.WriteLine("</kml>");
			GoogleEarthKMLFile.Close();
			if (Path.GetExtension(CreatedFile)!.ToLower() != ".kml")
			{
				string text = Path.ChangeExtension(CreatedFile, ".kml");
				if (File.Exists(text))
				{
					File.Delete(text);
				}
				File.Move(CreatedFile, text);
				CreatedFile = text;
			}
			if (ConvertToKMZ)
			{
				string text2 = Path.ChangeExtension(CreatedFile, ".kmz");
				if (File.Exists(text2))
				{
					File.Delete(text2);
				}
				using (ZipArchive destination = ZipFile.Open(text2, ZipArchiveMode.Create))
				{
					destination.CreateEntryFromFile(CreatedFile, new FileInfo(CreatedFile).Name);
				}
				File.Delete(CreatedFile);
				CreatedFile = text2;
			}
			return CreatedFile;
		}

		internal static void DisplayGoogleMap(string CreatedFile)
		{
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0023: Invalid comparison between Unknown and I4
			try
			{
				Process.Start(CreatedFile);
			}
			catch
			{
				if ((int)MessageBox.Show("Occult could not open GoogleEarth to display the prediction\r\nbecause GoogleEarth has not been installed on this computer.\r\n\r\nMaintenance / User Settings has a location to indicate if GoogleEarth has been installed\r\n\r\nDo you want to record that GoogleEarth is not installed on this computer?", "GoogleEarth not found", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152) != 7)
				{
					Settings.Default.GoogleEarthInstalled = false;
				}
			}
		}

		public static void CreateGoogleSkyKMLFile(double RA_deg, double Dec_deg, int width_in_arcMins, string OutFile)
		{
			double num = RA_deg - 180.0;
			if (num > 180.0)
			{
				num -= 360.0;
			}
			if (num < -180.0)
			{
				num += 360.0;
			}
			using (StreamWriter streamWriter = new StreamWriter(OutFile, append: false))
			{
				streamWriter.WriteLine("<kml xmlns=\"http://www.opengis.net/kml/2.2\" hint=\"target=sky\">");
				streamWriter.WriteLine("  <Document>");
				streamWriter.WriteLine("   <Placemark>");
				streamWriter.WriteLine("    <LookAt>");
				streamWriter.WriteLine("      <longitude>" + string.Format("{0,2:F6}", num) + "</longitude>");
				streamWriter.WriteLine("      <latitude>" + string.Format("{0,2:F6}", Dec_deg) + "</latitude>");
				streamWriter.WriteLine("      <altitude>0</altitude>");
				streamWriter.WriteLine("      <range>" + string.Format("{0,2:F0}", 1140 * width_in_arcMins) + "</range>");
				streamWriter.WriteLine("      <tilt>0</tilt>");
				streamWriter.WriteLine("      <heading>0</heading>");
				streamWriter.WriteLine("    </LookAt>");
				streamWriter.WriteLine("  ");
				streamWriter.WriteLine("    <Point>");
				streamWriter.WriteLine("      <coordinates>" + string.Format("{0,1:F6}", num) + "," + string.Format("{0,1:F6}", Dec_deg) + ",0</coordinates>");
				streamWriter.WriteLine("    </Point>");
				streamWriter.WriteLine("   </Placemark>");
				streamWriter.WriteLine("  </Document>");
				streamWriter.WriteLine("</kml>");
			}
			DisplayGoogleMap(OutFile);
		}

		public static bool Create_New_GoogleMap_File(string OutFile, string PageName, bool PositiveTimeIncrement, bool IncludeDirectionArrow, bool AutoOpenFile)
		{
			//IL_0009: Unknown result type (might be due to invalid IL or missing references)
			//IL_000f: Expected O, but got Unknown
			//IL_0071: Unknown result type (might be due to invalid IL or missing references)
			//IL_0077: Invalid comparison between Unknown and I4
			string path = OutFile;
			if (!AutoOpenFile)
			{
				SaveFileDialog val = new SaveFileDialog();
				((FileDialog)val).set_CheckPathExists(true);
				((FileDialog)val).set_Title(OutFile);
				val.set_OverwritePrompt(true);
				try
				{
					((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.FilePathGoogleMaps));
				}
				catch
				{
				}
				((FileDialog)val).set_FileName(OutFile.Replace(" ", "_") + ".htm");
				((FileDialog)val).set_Filter("htm files (*.htm)|*.htm|All files (*.*)|*.*");
				((FileDialog)val).set_FilterIndex(0);
				if ((int)((CommonDialog)val).ShowDialog() != 1)
				{
					Settings.Default.FilePathGoogleMaps = ((FileDialog)val).get_FileName();
					return false;
				}
				path = ((FileDialog)val).get_FileName();
			}
			try
			{
				GoogleMapFile = new StreamWriter(path, append: false);
				StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\GoogleMap_FileHeader.txt");
				GoogleMapFile.Write(streamReader.ReadToEnd());
				streamReader.Close();
				GoogleMapFile.WriteLine("page_name_string = \"" + PageName + "\"");
				if (IncludeDirectionArrow)
				{
					if (PositiveTimeIncrement)
					{
						GoogleMapFile.WriteLine("reverse_time_order = 0");
					}
					else
					{
						GoogleMapFile.WriteLine("reverse_time_order = 1");
					}
				}
				else
				{
					GoogleMapFile.WriteLine("reverse_time_order = -1");
				}
			}
			catch
			{
				return false;
			}
			PolyPointsExist = false;
			return true;
		}

		public static void Write_Tags_For_New_GoogleMap_Polyline_Path()
		{
			GoogleMapFile.WriteLine("var polypts = [");
			GoogleMapFirstOutput = true;
			LastLongitude = (LastLatitude = 0.0);
		}

		public static void Write_PolyLine_Path_Coordinate_GoogleMap(double Longitude, double Latitude, int Color)
		{
			double num = 179.99;
			double num2 = 0.0;
			double num3 = 0.0;
			if (!GoogleMapFirstOutput)
			{
				if (Math.Abs(Longitude - LastLongitude) > 180.0 || Longitude == 180.0)
				{
					if (Longitude == 180.0)
					{
						num2 = (num3 = Latitude);
					}
					else
					{
						double num4 = 360.0 - Math.Abs(Longitude - LastLongitude);
						num2 = LastLatitude + (Latitude - LastLatitude) * (180.0 - Math.Abs(LastLongitude)) / num4;
						num3 = Latitude - (LastLatitude - Latitude) * (180.0 - Math.Abs(Longitude)) / num4;
					}
					GoogleMapFile.WriteLine(",");
					if (LastLongitude < 0.0)
					{
						num = 0.0 - num;
					}
					GoogleMapFile.Write(string.Format("new google.maps.LatLng({0,6:F4},{1,6:F4})", num2, num));
					GoogleMapFile.WriteLine("\r\n];");
					GoogleMapFile.WriteLine("var polyline = new google.maps.Polyline( {");
					GoogleMapFile.WriteLine("    path: polypts,");
					GoogleMapFile.WriteLine("    geodesic: true,");
					if (Color == -1)
					{
						GoogleMapFile.WriteLine("    strokeColor: '#00FF00',");
					}
					else if (Color == 0 || Color == 1)
					{
						GoogleMapFile.WriteLine("    strokeColor: '#0000FF',");
					}
					else if (Color == 2 || Color == 3)
					{
						GoogleMapFile.WriteLine("    strokeColor: '#FF0000',");
					}
					else
					{
						GoogleMapFile.WriteLine("    strokeColor: '#FFFF00',");
					}
					GoogleMapFile.WriteLine("    strokeOpacity: 0.5,");
					GoogleMapFile.WriteLine("    strokeWeight: 2");
					GoogleMapFile.WriteLine("});");
					GoogleMapFile.WriteLine("polyline.setMap(map);");
					GoogleMapFile.WriteLine("");
					GoogleMapFile.WriteLine("var polypts = [");
					GoogleMapFile.Write(string.Format("new google.maps.LatLng({0,6:F4},{1,6:F4})", num3, 0.0 - num));
					GoogleMapFirstOutput = true;
					Longitude = 0.0 - num;
				}
				else
				{
					if (!GoogleMapFirstOutput)
					{
						GoogleMapFile.WriteLine(",");
					}
					GoogleMapFile.Write(string.Format("new google.maps.LatLng({0,6:F4},{1,6:F4})", Latitude, Longitude));
				}
			}
			else if (Longitude != 180.0)
			{
				if (!GoogleMapFirstOutput)
				{
					GoogleMapFile.WriteLine(",");
				}
				GoogleMapFile.Write(string.Format("new google.maps.LatLng({0,6:F4},{1,6:F4})", Latitude, Longitude));
			}
			GoogleMapFirstOutput = false;
			LastLongitude = Longitude;
			LastLatitude = Latitude;
		}

		public static void Write_Tags_At_End_of_Polyline_Path_GoogleMap(int Color)
		{
			Settings.Default.GoogleMapsLineWidth.ToString();
			GoogleMapFile.WriteLine("\r\n];");
			GoogleMapFile.WriteLine("var polyline = new google.maps.Polyline( {");
			GoogleMapFile.WriteLine("    path: polypts,");
			GoogleMapFile.WriteLine("    geodesic: true,");
			if (Color == -1)
			{
				GoogleMapFile.WriteLine("    strokeColor: '#00FF00',");
			}
			else if (Color == 0 || Color == 1)
			{
				GoogleMapFile.WriteLine("    strokeColor: '#0000FF',");
			}
			else if (Color == 2 || Color == 3)
			{
				GoogleMapFile.WriteLine("    strokeColor: '#FF0000',");
			}
			else
			{
				GoogleMapFile.WriteLine("    strokeColor: '#FFFF00',");
			}
			GoogleMapFile.WriteLine("    strokeOpacity: 0.5,");
			GoogleMapFile.WriteLine("    strokeWeight: 2");
			GoogleMapFile.WriteLine("});");
			GoogleMapFile.WriteLine("polyline.setMap(map);");
			GoogleMapFile.WriteLine("");
		}

		public static void Write_Tags_For_New_GoogleMap_PolyPoint_Path()
		{
			GoogleMapFile.WriteLine("var polypts = [");
			GoogleMapFirstOutput = true;
			PolypointCounter = 0;
		}

		public static void Write_PolyPoint_Path_Coordinate_GoogleMap(double Longitude, double Latitude)
		{
			if (!GoogleMapFirstOutput)
			{
				GoogleMapFile.WriteLine(",");
			}
			GoogleMapFile.Write(string.Format("new google.maps.LatLng({0,6:F4},{1,6:F4})", Latitude, Longitude));
			PolypointCounter++;
			GoogleMapFirstOutput = false;
		}

		public static void Write_Tags_At_End_of_Polypoint_Path_GoogleMap(int Color)
		{
			GoogleMapFile.WriteLine("];");
			GoogleMapFile.WriteLine(string.Format("var polypts_cnt = {0,4:F0}", PolypointCounter));
			GoogleMapFile.WriteLine("");
			PolyPointsExist = true;
		}

		public static void Close_GoogleMap_File(double CentreLongitude, double CentreLatitude)
		{
			if (!PolyPointsExist)
			{
				GoogleMapFile.WriteLine("");
				GoogleMapFile.WriteLine("var polypts_cnt = 0;");
				GoogleMapFile.WriteLine("");
			}
			GoogleMapFile.WriteLine("");
			GoogleMapFile.WriteLine($"var ctr_lat={CentreLatitude:F1}; //Centre latitude when no polypts array provided");
			GoogleMapFile.WriteLine($"var ctr_lon={CentreLongitude:F1}; //Centre longitude when no polypts array provided");
			GoogleMapFile.WriteLine("");
			try
			{
				StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\GoogleMap_FileFooter.txt");
				GoogleMapFile.Write(streamReader.ReadToEnd());
				streamReader.Close();
			}
			catch
			{
			}
			GoogleMapFile.Close();
		}

		public static bool Create_New_MIF_File(string OutFile)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0068: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_Title(OutFile);
			val.set_OverwritePrompt(true);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.FilePathMapsMIF));
			}
			catch
			{
			}
			((FileDialog)val).set_FileName(OutFile.Replace(" ", "_") + ".mif");
			((FileDialog)val).set_Filter("mif files (*.mif)|*.mif|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Settings.Default.FilePathMapsMIF = ((FileDialog)val).get_FileName();
				MIF_file = new StreamWriter(((FileDialog)val).get_FileName(), append: false);
				MIF_file.WriteLine("Version 300");
				MIF_file.WriteLine("Charset \"WindowsLatin1\"");
				MIF_file.WriteLine("Delimiter \",\"");
				MIF_file.WriteLine("CoordSys Earth Projection 1, 0");
				MIF_file.WriteLine("Columns 0");
				MIF_file.WriteLine("");
				MIF_file.WriteLine("Data");
				MIFpoints = new ArrayList();
				return true;
			}
			return false;
		}

		public static void Add_point_to_MIF_Line(double Longitude, double Latitude)
		{
			MIFpoints.Add(string.Format("{0,6:F4} {1,6:F4}", Longitude, Latitude));
		}

		public static void Write_MIF_Line_Block(int Color)
		{
			int count = MIFpoints.Count;
			MIF_file.WriteLine(string.Format("Pline {0,1:F0}", count));
			for (int i = 0; i < count; i++)
			{
				MIF_file.WriteLine(MIFpoints[i]!.ToString());
			}
			if (Color == -1)
			{
				MIF_file.WriteLine("Pen (1,7,255)");
			}
			else if (Color == 0 || Color == 1)
			{
				MIF_file.WriteLine("Pen (1,2,255)");
			}
			else if (Color == 2 || Color == 3)
			{
				MIF_file.WriteLine("Pen (1,2,16711680)");
			}
			MIFpoints = new ArrayList();
		}

		public static void Close_MIF_File()
		{
			MIF_file.Close();
		}

		public static bool Create_New_CMX_File(string OutFile, out string SavedFileName)
		{
			//IL_0007: Unknown result type (might be due to invalid IL or missing references)
			//IL_000d: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0075: Invalid comparison between Unknown and I4
			SavedFileName = "";
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_Title(OutFile);
			val.set_OverwritePrompt(true);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.FilePathMapsCMX));
			}
			catch
			{
			}
			((FileDialog)val).set_FileName(OutFile.Replace(" ", "_") + ".CMX");
			((FileDialog)val).set_Filter("CMX files (*.CMX)|*.CMX|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Settings.Default.FilePathMapsCMX = ((FileDialog)val).get_FileName();
				SavedFileName = ((FileDialog)val).get_FileName();
				CMX_file = new StreamWriter(((FileDialog)val).get_FileName(), append: false);
				CMX_file.WriteLine("CHICAGO MAP EXCHANGE FORMAT v40");
				CMX_file.WriteLine("REAL");
				CMX_file.WriteLine("DATA");
				CMX_file.WriteLine("");
				CMXpoints = new ArrayList();
				return true;
			}
			return false;
		}

		public static void Add_point_to_CMX_Line(double Longitude, double Latitude)
		{
			CMXpoints.Add($"{Longitude:F6},{Latitude:F6}");
		}

		public static void Write_CMX_Line_Block(int Color, bool LastBlock)
		{
			int count = CMXpoints.Count;
			CMX_file.WriteLine(string.Format("Pline {0,1:F0}", count));
			for (int i = 0; i < count; i++)
			{
				CMX_file.WriteLine(CMXpoints[i]!.ToString());
			}
			if (Color == -1)
			{
				CMX_file.WriteLine(" Pen(4,1,0)");
			}
			else if (Color == 0 || Color == 1)
			{
				CMX_file.WriteLine(" Pen(0,2,12)");
			}
			else if (Color == 2 || Color == 3)
			{
				CMX_file.WriteLine(" Pen(1,1,9)");
			}
			if (LastBlock)
			{
				CMX_file.Write(" Brush(0,0)");
			}
			else
			{
				CMX_file.WriteLine(" Brush(0,0)");
			}
			CMXpoints = new ArrayList();
		}

		public static void Close_CMX_File()
		{
			CMX_file.Close();
		}

		public static bool Create_New_GPX_File(string OutFile)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0068: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_Title(OutFile);
			val.set_OverwritePrompt(true);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.FilePathMapsGPX));
			}
			catch
			{
			}
			((FileDialog)val).set_FileName(OutFile.Replace(" ", "_") + ".GPX");
			((FileDialog)val).set_Filter("GPX files (*.GPX)|*.GPX|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Settings.Default.FilePathMapsGPX = ((FileDialog)val).get_FileName();
				GPX_file = new StreamWriter(((FileDialog)val).get_FileName(), append: false);
				GPX_file.WriteLine("<?xml version=\"1.0\" standalone=\"yes\"?>");
				GPX_file.WriteLine("<gpx version=\"1.0\" creator=\"winOccult program\"");
				GPX_file.WriteLine(" xmlns=\"http://www.topografix.com/GPX/1/0\"");
				GPX_file.WriteLine(" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"");
				GPX_file.WriteLine(" xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">");
				GPX_file.WriteLine("<trk>");
				GPXpoints = new ArrayList();
				return true;
			}
			return false;
		}

		public static void Add_point_to_GPX_Line(double Longitude, double Latitude)
		{
			GPXpoints.Add($"<trkpt lat=\"{Latitude:F4}\" lon=\"{Longitude:F4}\"> </trkpt>");
		}

		public static void Write_GPX_Line_Block()
		{
			int count = GPXpoints.Count;
			GPX_file.WriteLine("<trkseg>");
			for (int i = 0; i < count; i++)
			{
				GPX_file.WriteLine(GPXpoints[i]!.ToString());
			}
			GPX_file.WriteLine("</trkseg>");
			GPXpoints = new ArrayList();
		}

		public static void Close_GPX_File()
		{
			GPX_file.WriteLine("</trk>");
			GPX_file.WriteLine("</gpx>");
			GPX_file.Close();
		}

		public static bool Create_New_PLT_File(string OutFile)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0068: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_Title(OutFile);
			val.set_OverwritePrompt(true);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.FilePathMapsPLT));
			}
			catch
			{
			}
			((FileDialog)val).set_FileName(OutFile.Replace(" ", "_") + ".PLT");
			((FileDialog)val).set_Filter("PLT files (*.PLT)|*.PLT|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Settings.Default.FilePathMapsPLT = ((FileDialog)val).get_FileName();
				PLT_file = new StreamWriter(((FileDialog)val).get_FileName(), append: false);
				PLT_file.WriteLine("OziExplorer Track Point File Version 2.1");
				PLT_file.WriteLine("WGS 84");
				PLT_file.WriteLine("Altitude is in Feet");
				PLT_file.WriteLine("Reserved 3");
				PLT_file.WriteLine("0,2, 255,Asteroid Shadow Path,1,0,0, 255");
				PLTpoints = new ArrayList();
				return true;
			}
			return false;
		}

		public static void Add_point_to_PLT_Line(double Longitude, double Latitude, int FirstPointInLine)
		{
			PLTpoints.Add($"{Latitude:F4},{Longitude:F4},{FirstPointInLine:F0},0,,,");
		}

		public static void Write_PLT_Line_Block()
		{
			PLT_file.WriteLine(string.Format(" {0,1:F0}", PLTpoints.Count));
			int count = PLTpoints.Count;
			for (int i = 0; i < count; i++)
			{
				PLT_file.WriteLine(PLTpoints[i]!.ToString());
			}
		}

		public static void Close_PLT_File()
		{
			PLT_file.Close();
		}

		public static bool Create_New_GEN_File(string OutFile)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0068: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_Title(OutFile);
			val.set_OverwritePrompt(true);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.FilePathMapsGEN));
			}
			catch
			{
			}
			((FileDialog)val).set_FileName(OutFile.Replace(" ", "_") + ".GEN");
			((FileDialog)val).set_Filter("GEN files (*.GEN)|*.GEN|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Settings.Default.FilePathMapsGEN = ((FileDialog)val).get_FileName();
				GEN_file = new StreamWriter(((FileDialog)val).get_FileName(), append: false);
				GENpoints = new ArrayList();
				return true;
			}
			return false;
		}

		public static void Add_point_to_GEN_Line(double Longitude, double Latitude)
		{
			GENpoints.Add($"{Longitude:F4},{Latitude:F4}");
		}

		public static void Write_GEN_Line_Block(int BlockNum)
		{
			GEN_file.WriteLine(string.Format("{0,1:F0}", BlockNum));
			int count = GENpoints.Count;
			for (int i = 0; i < count; i++)
			{
				GEN_file.WriteLine(GENpoints[i]!.ToString());
			}
			GEN_file.WriteLine("end");
			GENpoints = new ArrayList();
		}

		public static void Close_GEN_File()
		{
			GEN_file.WriteLine("end");
			GEN_file.Close();
		}

		public static bool Create_New_DeLorme_File(string OutFile)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0068: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_CheckPathExists(true);
			((FileDialog)val).set_Title(OutFile);
			val.set_OverwritePrompt(true);
			try
			{
				((FileDialog)val).set_InitialDirectory(Path.GetDirectoryName(Settings.Default.FilePathMapsDeLorme));
			}
			catch
			{
			}
			((FileDialog)val).set_FileName(OutFile.Replace(" ", "_") + "_SA.txt");
			((FileDialog)val).set_Filter("Delorme files (*.txt)|*.txt|All files (*.*)|*.*");
			((FileDialog)val).set_FilterIndex(0);
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				Settings.Default.FilePathMapsDeLorme = ((FileDialog)val).get_FileName();
				DeLorme_file = new StreamWriter(((FileDialog)val).get_FileName(), append: false);
				DeLormepoints = new ArrayList();
				return true;
			}
			return false;
		}

		public static void Add_point_to_DeLorme_Line(double Longitude, double Latitude)
		{
			DeLormepoints.Add($"{Latitude:F4},{Longitude:F4}");
		}

		public static void Write_DeLorme_Line_Block()
		{
			DeLorme_file.WriteLine("BEGIN LINE");
			int count = DeLormepoints.Count;
			for (int i = 0; i < count; i++)
			{
				DeLorme_file.WriteLine(DeLormepoints[i]!.ToString());
			}
			DeLorme_file.WriteLine("END");
			DeLormepoints = new ArrayList();
		}

		public static void Close_DeLorme_File()
		{
			DeLorme_file.Close();
		}
	}
}
