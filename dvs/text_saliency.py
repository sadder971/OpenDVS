import cv2
import numpy as np
from skimage.measure import label, regionprops
from skimage.morphology import skeletonize
from scipy.ndimage import distance_transform_edt
import matplotlib.pyplot as plt
from math import sqrt



def textF1(grayimg, boxes):
    
    # Computing the gradient along the X and Y axis using the Sobel operator, 
    # the Sobel computes an approximation of the gradient of an image
    # intensity function
    gX = cv2.Sobel(grayimg, ddepth = cv2.CV_32F, dx = 1, dy = 0, ksize = -1)
    gY = cv2.Sobel(grayimg, ddepth = cv2.CV_32F, dx = 0, dy = 1, ksize = -1)
    
    # Convert output to a CV_8U image
    gX = cv2.convertScaleAbs(gX)
    gY = cv2.convertScaleAbs(gY)
    
    # Combine the X and u graidents to get a single gradient magnitude map
    gmag = cv2.addWeighted(gX, 0.5, gY, 0.5, 0)
    # cv2.imshow("Sobel/Scharr X", gX)
    # cv2.imshow("Sobel/Scharr Y", gY)
    # cv2.imshow("Sobel/Scharr Combined", combined)
    # cv2.waitKey(0)
    
    
    F = np.zeros_like(grayimg, dtype = float)
    
    for box in boxes:
        ymin, ymax, xmin, xmax = box
        
        # Calculate the center of the bounding box
        xcent = round((xmin + xmax) / 2)
        ycent = round((ymin + ymax) / 2)
        
        # Extract the patch of the gradient magnitude map corresponding to the bouding box 
        patch = gmag[ymin:ymax, xmin:xmax]
        
        # Calculate the feature for this patch, mean / sd
        F[ycent, xcent] = (np.mean(patch) / (np.std(patch) + np.finfo(float).eps))
        
    return F




def patchF2(patch):
    H, W = patch.shape
    R = 4
    
    # Count the rows with sum greater than 2
    f1 = np.sum(np.sum(patch, axis = 1) > 2)
    
    # Count the columns with sum greater than 2
    f2 = np.sum(np.sum(patch, axis = 0) > 2)
    
    # Compute the feature
    f = R ** ((f1 + f2) / (W + H))
    
    return f




def patchF3(patch):
    H, W = patch.shape
    P = 1.22
    
    # Count the rows with even sum of values
    g1 = np.sum(np.sum(patch, axis = 1) % 2 == 0)
    
    # Count the columns with even sum of values
    g2 = np.sum(np.sum(patch, axis = 0) % 2 == 0)
    
    # Compute the feature
    g = P ** ((g1 + g2) / (W + H))
    
    return g 


def normalize01(map):
  return (map- map.min()) / (map.max() - map.min())



def textF2F3(img, boxes):
  
    # Check whether the image is colored (3 channels)
    if img.ndim == 3 and img.shape[2] == 3:
        # Split the color image into L, A and B channels
        L, A, B = cv2.split(img)
        
        # Normalizze each channel and convert to 8-bit unsigned integer
        L = (normalize01(L)).astype(np.uint8)
        A = (normalize01(A)).astype(np.uint8)
        B = (normalize01(B)).astype(np.uint8)
        
        # Apply Canny edge detection to each channel to detetc edges
        BWl = cv2.Canny(L, 50, 100)
        BWa = cv2.Canny(A, 50, 100)
        BWb = cv2.Canny(B, 50, 100)
        
        F2 = np.zeros(img.shape[:2], dtype = float)
        F3 = np.zeros(img.shape[:2], dtype = float)
        
        for box in boxes:
            ymin, ymax, xmin, xmax = box

            # Calculating the center of the bounding box
            xcent = round((xmin + xmax) / 2)
            
            ycent = round((ymin + ymax) / 2)
            
            # Ensure the center coordinates are within image bounds
            if 0 <= xcent < F2.shape[1] and 0 <= ycent < F2.shape[0]:
              
                # Extract patches from each edge-detected channel
                patch1 = BWl[ymin:ymax, xmin:xmax]
                patch2 = BWa[ymin:ymax, xmin:xmax]
                patch3 = BWb[ymin:ymax, xmin:xmax]
                
                # Calculate features for each patch and Update F2 and F3 feature maps
                # The features are averaged across all three channels
                F2[ycent, xcent] = (patchF2(patch1) + patchF2(patch2) + patchF2(patch3)) / 3
                
                F3[ycent, xcent] = (patchF3(patch1) + patchF3(patch2) + patchF3(patch3)) / 3
    
    # If the image is gray scale 
    elif img.ndim == 2 or (img.ndim == 3 and img.shape[2] == 1):
        
        # Normalizing the gray scale image
        img = normalize01(img)
        
        # Apply Canny edge detection 
        BW = cv2.Canny(img, 50, 100)  
        
        for i, box in enumerate(boxes):
            xmin, ymin, xmax, ymax = box
            
            # Calculating the center of the bounding box
            xcent = round((xmin + xmax) / 2)
            ycent = round((ymin + ymax) / 2)
            
            if 0 <= xcent < F2.shape[1] and 0 <= ycent < F2.shape[0]:
                # Extracting the patch corresponding to the bounding box
                patch = BW[ymin:ymax, xmin:xmax]
                
                # Calculate and update the features in F2 and F3 feature maps for the patch
                F2[ycent, xcent] = patchF2(patch)
                F3[ycent, xcent] = patchF3(patch)

    return F2, F3



def preprocess_image(gray_img, lab_img, scale):
    width = int(gray_img.shape[1] * scale)
    height = int(gray_img.shape[0] * scale)
    dim = (width, height)

    scaled_gray_img = cv2.resize(gray_img, dim, interpolation=cv2.INTER_CUBIC)
    scaled_lab_img = cv2.resize(lab_img, dim, interpolation=cv2.INTER_CUBIC) 
    return scaled_gray_img, scaled_lab_img
  
  
def detect_text_regions(img, scale):
    mser = cv2.MSER_create()
    mser.setMinArea(int(20 * scale))
    mser.setMaxArea(int(8000 * scale))
    mser.setDelta(1)
    
    regions, _ = mser.detectRegions(img)
    return regions


def filter_and_normalize_regions(regions, scale, img_shape):
    original_bboxes = []
    H, W = img_shape[:2]
    
    for region in regions:
        # Create a mask for the region
        region_mask = np.zeros((H, W), dtype=np.uint8)
        region_mask[region[:, 1], region[:, 0]] = 1
        
        # Apply label to find properties of the region
        label_img = label(region_mask)
        props = regionprops(label_img)[0]
        
        # Filter based on geometric properties
        minr, minc, maxr, maxc = props.bbox
        aspect_ratio = (maxc - minc) / (maxr - minr)
        filter_idx = (aspect_ratio > 3 or 
                props.eccentricity > 0.995 or 
                props.solidity < 0.3 or 
                props.extent < 0.2 or 
                props.extent > 0.9 or 
                props.euler_number < -4)
            
        if filter_idx:
            continue
        
        # Calculate stroke width variation
        region_mask = np.pad(region_mask, ((1, 1), (1, 1)), "constant", constant_values=0)
        distance_image = distance_transform_edt(~region_mask)
        skeleton = skeletonize(region_mask)
        stroke_width_values = distance_image[skeleton.astype(bool)]
        strokeWidthMetric = np.std(stroke_width_values) / np.mean(stroke_width_values)
        strokeWidthThreshold = 0.3
        
        if strokeWidthMetric < strokeWidthThreshold: 

           ymin, xmin = np.floor([minr * 0.98 - 2, minc * 0.98 - 2]).astype(int)
           xmax, ymax = np.ceil([(xmin + round((maxc - minc)) * 1.02 + 1), 
                                  (ymin + round((maxr - minr)) * 1.02 + 1)]).astype(int)
                
           xmin, ymin = np.maximum([xmin, ymin], 1)
           xmax, ymax = np.minimum([xmax, ymax], [img_shape[1], img_shape[0]])
                
           original_ymin = int(ymin / scale)
           original_xmin = int(xmin / scale)
           original_ymax = int(ymax / scale)
           original_xmax = int(xmax / scale)
        
           original_xmin, original_ymin = np.maximum([original_xmin, original_ymin], 0)
           original_xmax, original_ymax = np.minimum([original_xmax, original_ymax], [W - 1, H - 1])
        
           original_bboxes.append((original_ymin, original_ymax, original_xmin, original_xmax))
        
    return original_bboxes



def compute_featuremaps(gray_img, lab_img, boxes):
    F1 = textF1(gray_img, boxes)
    F2, F3 = textF2F3(lab_img, boxes)
    return F1, F2, F3
  
  
def aggregate_and_normalize_saliency(F1, F2, F3, S, gray_img):
    H, W = gray_img.shape[:2]
    F1 = F1 * 2.5
    F1 = F1 * 10
    F2 = F2 * 10
    F3 = F3 * 10
    S0 = F1 + F2 + F3
    
    S0 = cv2.GaussianBlur(S0, (0, 0), sigmaX = sqrt(H * W) / 52, sigmaY = 0)
    S0_resized = cv2.resize(S0, (S.shape[1], S.shape[0]), interpolation = cv2.INTER_CUBIC)
    
    return S0_resized
  
  
  
def textSaliency(img_path):
    gray_img = cv2.imread(img_path, 0)
    color_img = cv2.imread(img_path)
    lab_img = cv2.cvtColor(color_img, cv2.COLOR_BGR2LAB)

    S = np.zeros(gray_img.shape[:2], dtype = np.float32)

    scales = np.arange(0.4, 1.5, 0.2)
    for scale in scales:
        scaled_gray_img, scaled_lab_img = preprocess_image(gray_img, lab_img, scale)
        regions = detect_text_regions(scaled_gray_img, scale)
        boxes = filter_and_normalize_regions(regions, scale, scaled_gray_img.shape)

    F1, F2, F3 = compute_featuremaps(gray_img, lab_img, boxes)
    S0_resized = aggregate_and_normalize_saliency(F1, F2, F3, S, gray_img)
    S += S0_resized

    S /= len(scales)
    S[S <= 0] = np.finfo(float).eps
    S = (S - S.min()) / (S.max() - S.min())

    return S






